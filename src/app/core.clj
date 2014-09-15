(ns app.core
  (import (org.eclipse.jdt.core.dom AST ASTParser ASTNode IBinding Modifier)
          (org.eclipse.jdt.core JavaCore)
          (java.io File))
  (:require [clojure.string :as str]))

(def type-method "m")
(def type-var "v")

(def osx-java-core "/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar")

(def jdk-option-map 
  {:jdk5 JavaCore/VERSION_1_5
   :jdk6 JavaCore/VERSION_1_6
   :jdk7 JavaCore/VERSION_1_7
   })

(defn detect-java-core
  "Attempt to locate java core jar.
  Returns a vector of jars (usually one)"
  []
  (let [java-home (System/getenv "JAVA_HOME")
        java-home-jar (File. java-home "jre/lib/rt.jar")]
    (cond 
      (.exists java-home-jar)
        [(.getAbsolutePath java-home-jar)]
      (.exists (File. osx-java-core))
        [osx-java-core])))

(defn detect-environment
  "Detect the classpath and sourcepath
  for a given file"
  [path]
  (let [parts (.split path File/separator)
        ;; pathDir (str/join File/separator (drop-last parts))]
        ; FIXME actually do this; ensure trailing slash!
        pathDir "test/app/src/"
        unit (last parts)
        absPath (.getAbsolutePath (File. "test/app/src/"))]
    [(concat (detect-java-core)) ; TODO add other jars
     (map #(str % File/separator) [absPath])                   
     (str/join (drop-last 5 unit))]))

(defn create-ast
  "Create AST from args:
    :text Text contents of file
    :path Path to the file
    :with Specify JDK level to parse with; 
          any of :jdk5 :jdk6 :jdk7 (defaults to :jdk7)
  Returns a Map:
    :ast CompilationUnit instance
    :cp vector of classpath locations
    :sp vector of sourcepath locations
  "
  [& {:keys [text path with]
      :or {with :jdk7}}]
  (let [parser (ASTParser/newParser(AST/JLS4))
        [cp sp unit] (detect-environment path)
        acp (into-array java.lang.String cp)
        asp (into-array java.lang.String sp)
        opts (JavaCore/getOptions)]
    (JavaCore/setComplianceOptions (with jdk-option-map) opts)
    (doto parser
      (.setCompilerOptions opts)
      (.setSource (.toCharArray text))
      (.setStatementsRecovery true)
      (.setEnvironment acp asp nil false)
      (.setUnitName unit)
      (.setResolveBindings true))
    {:ast (.createAST parser nil)
     :cp cp
     :sp sp}))

(defn read-ast
  "Read an AST from a file. See create-ast"
  [path & opts]
  (when-let [text (slurp path)]
    (apply create-ast :text text :path path opts)))

(defn find-node
  "Search for an expression in the AST
  at the given line, col"
  [env line col]
  (let [ast (:ast env)
        pos (.getPosition ast line col)
        deepest (ref nil)
        visitor 
          (proxy [org.eclipse.jdt.core.dom.ASTVisitor] []
            (visit [node] 
              (let [start (.getStartPosition node)
                    length (.getLength node)
                    end (+ start length) ]
                (if (<= start pos end)
                  (do (dosync (ref-set deepest node))
                    true)
                  false)))
          )]
   (.accept ast visitor)
   @deepest))

(defn find-expr
  "Get the closest StatementExpression 
  at the given line, col"
  [env line col]
  (let [node (find-node env line col)]
    (loop [curr node]
      (if (= ASTNode/EXPRESSION_STATEMENT (.getNodeType curr))
        curr
        (recur (.getParent curr))))))

(defn extract-method
  "Extract info from a method
  into a map"
  [method]
  {:what type-method
   :name (.getName method)
   :return (-> method .getReturnType .getQualifiedName)
   :constructor (.isConstructor method)
   })

(defn extract-var
  "Extract info from a variable
  into a map"
  [obj]
  {:what type-var
   :name (.getName obj)
   :type (-> obj .getType .getQualifiedName)
   :static (not= 0 (bit-and (.getModifiers obj) Modifier/STATIC))
   })

(defn read-ast-class
  "Given a fully-qualified name and an environment,
  attempt to read an ast"
  [env fqn]
  (let [fqn-file (str (.replace fqn "." "/" ) ".java")
        candidates (map #(File. %1 %2) (:sp env) (repeat fqn-file))]
    (when-let [match (.getAbsolutePath (first (filter #(.exists %) candidates)))]
      (read-ast match))))

(defn- extract-javadoc-internal-ast
  "Given an ast env and a predicate, find
  the node and get the javadoc"
  [env pred]
  (when-let [jd-obj (.getJavadoc (pred (:ast env)) )]
    ; get a coll of all fragments in all tags
    (let [tags (.tags jd-obj)
          sections (map (fn [tag]
                          (cons (.getTagName tag) (.fragments tag))
                          ) tags)
          flat (filter identity (flatten sections)) ; non-nil plz
          parts (map 
                  (fn [el]
                    (if (string? el)
                      (str el " ") ; eg: "@return"
                      (str (-> el .toString .trim) "\n"))) ; any TextElement
                  flat)
          ]
      (.trim (str/join parts)))))
 
(defn- extract-javadoc-internal
  "Expects an ITypeBinding and a fn
  to extract the node. This is mostly a convenience
  if you don't already have an ast instance"
  [env binder pred]
  {:pre [(identity binder)]} ; nil check
  (extract-javadoc-internal-ast 
    (read-ast-class env (.getQualifiedName binder)) 
    pred))

(defmulti extract-javadoc
  "Extract javadoc from a method, etc"
  (fn [env node] 
    {:pre [(instance? IBinding node)]}
    (.getKind node)))
(defmethod extract-javadoc IBinding/METHOD
  [env node]
  (let [binder (.getDeclaringClass node)
        pred #(.findDeclaringNode % (.getKey node))]
    (extract-javadoc-internal env binder pred)))
(defmethod extract-javadoc IBinding/VARIABLE
  [env node]
  (when (.isField node)
    (let [binder (.getDeclaringClass node)
          pred #(.getParent (.findDeclaringNode % (.getKey node)))]
      (extract-javadoc-internal env binder pred))))
;; (defmethod extract-javadoc :default
;;   [node]
;;   (str "has" (= (.getKind node) IBinding/METHOD))
;;   )

(def suggest-handlers
  {ASTNode/EXPRESSION_STATEMENT
   (fn [node]
     (when-let [bind (-> node .getExpression .resolveTypeBinding)]
       (let [meths (.getDeclaredMethods bind)
             fields (.getDeclaredFields bind)]
         (concat 
           (map extract-method meths) 
           (map extract-var fields))
         )))
   ; might not actually be anything else?
   })

;; ; for easy testing
;; (get-suggestions foo 8 16 )
;; (get-suggestions foo 12 30 )
;; (get-suggestions foo 17 9 )
;; (get-suggestions foo 21 12 )
;; (-> (find-node foo 21 13) 
;;     .getParent .getParent 
;;     .getExpression .getLeftHandSide .getIdentifier)

(defn handle-suggestion 
  "shortcut to picking and applying a handler for a node"
  [node]
  (loop [this-node node]
    (if-let [handler (suggest-handlers (.getNodeType this-node))]
      (handler this-node)
      (when (not= ASTNode/BLOCK (.getNodeType this-node))
        (recur (.getParent this-node))))))

(defn get-suggestions
  "Get suggestions at a position, given an AST"
  [env line col]
  (when-let [node (find-node env line col)]
    (handle-suggestion node)))

(defn- identify-var
  ; Hopefully merge with extract-var somehow
  [env node id]
  {:what type-var
   :name id
   :type (-> node .resolveTypeBinding .getQualifiedName)
   :javadoc (extract-javadoc env (.resolveBinding node))
   ;; :value (.resolveConstantExpressionValue node) ; TODO
   })

(defn identify
  "Get information about the AST node at a given position"
  [env line col]
  (when-let [node (find-node env line col) ]
    (let [parent (.getParent node)
          method-invoke (= ASTNode/METHOD_INVOCATION (.getNodeType parent))
          id (.getIdentifier node)]
      (cond 
        (and method-invoke (not= id (-> parent .getName .getIdentifier)))
          (identify-var env node id)

        (and method-invoke (= id (-> parent .getName .getIdentifier)))
          (let [m (.resolveMethodBinding parent)]
          ; TODO reuse extract-method?
            {:what type-method
             :name id
             :type (-> m .getDeclaringClass .getQualifiedName)
             :returns (-> m .getReturnType .getQualifiedName)
             :args (map #(.getQualifiedName %) (-> m .getParameterTypes))
             :javadoc (extract-javadoc env m)
             })

        (= ASTNode/QUALIFIED_NAME (.getNodeType parent))
          (identify-var env parent id)

        :else
          ; TODO method decl? class literal?
          {:binding (class (.resolveTypeBinding node))
           :pclass (class parent)
           ;; :qualifier (.resolveBinding parent)
           :class (class node)
           :ident (.getIdentifier node)
           :parent (-> parent .getName .getIdentifier)
           :invoke? method-invoke
           }
        ))))

(defn get-errors
  "Get array of error message info from ast"
  [env]
  (map (fn [obj]
         (let [ast (:ast env)
               start-pos (.getStartPosition obj)]
           {:text (.getMessage obj)
            :pos start-pos
            :line (.getLineNumber ast start-pos)
            :col (.getColumnNumber ast start-pos)
            :length (.getLength obj)
            })) 
       (.getMessages (:ast env))))

(defn get-missing-imports
  "Get array of missing imports from ast; another function can recommend..."
  [env]
  (when-let [relevant (filter #(.endsWith 
                                 (:text %) 
                                 "cannot be resolved to a type") 
                              (get-errors env))]
    (map #(first (str/split (:text %) #" ")) relevant)))

(defn get-unused-imports
  "Get array of unused imports from ast"
  [env]
  (when-let [relevant (map #(re-matches
                                 #"The import (.*) is never used"
                                 (:text %)) 
                              (get-errors env))]
    (map second (filter identity relevant))))

(defn wrapper
  "Wrap some code for testing in fireplace.vim"
  []

  (def src-path "test/app/src/net/dhleong/test/Foo.java")

  (def foo (read-ast src-path))

  (detect-environment src-path)

  ;; (def foo (create-ast : :text
  ;;            (apply str 
  ;;                   (interpose "\r" 
  ;;                              ["class Foo {"
  ;;                               "  void bar() {"
  ;;                               "    Magic m = new Magic();"
  ;;                               "    int i = 1;"
  ;;                               "    m.get().fo"
  ;;                               "  }"
  ;;                               "  void foo() {"
  ;;                               "     //nop"
  ;;                               "  }"
  ;;                               "}"]))))

  (.getName (.getParent (find-node foo 5, 13)))

  (.statements (.getBody (first (.getMethods (first (.types foo))))))

  (map #(println (.getLineNumber foo (.getStartPosition %)) (.getMessage %)) (.getMessages foo))

  ;; (org.eclipse.jdt.core.JavaCore/getOptions)

  )
