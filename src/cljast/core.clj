(ns cljast.core
  (import (org.eclipse.jdt.core.dom AST ASTParser ASTNode IBinding Modifier)
          (org.eclipse.jdt.core JavaCore)
          (java.io File))
  (:require [clojure.string :as str]
            [clojure.java.io :refer [file]]
            [cljast.gradle :as gradle]))

(def type-method "m")
(def type-var "v")

(def osx-java-core "/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar")

(def jdk-option-map 
  {:jdk5 JavaCore/VERSION_1_5
   :jdk6 JavaCore/VERSION_1_6
   :jdk7 JavaCore/VERSION_1_7
   })

(def java-source-dir-candidates
  "Places to look for src files in a project root"
  ["src/main/java" "src/release/java"])

(defn- java-source-dir-fallback
  [root coll]
  (let [fallback (file root "src")]
    (if (and (empty? coll) (.exists fallback))
      [fallback]
      coll)))

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

(defn detect-gradle-jars
  "Attempt to locate jar dependencies from a gradle project"
  [root-dir]
  (let [gradle-file (file root-dir "build.gradle")]
    (if (.exists gradle-file)
      (if (= -1 (-> (slurp gradle-file) (.indexOf "android")))
        ; not an android project; use the normal way
        (->> root-dir
             (gradle/project)
             (gradle/get-dependencies))
        ; android project... use the wacky fallback
        (gradle/classpath root-dir)))
    [])) ;; nothing 

(defn detect-project-root
  "Detect project root given a File"
  [file]
  (let [abs (.getAbsolutePath file)
        parts (str/split abs (re-pattern File/separator))
        src-index (.indexOf parts "src")
        root-parts (take src-index parts)]
    (str/join File/separator root-parts)))

(defn detect-depended-projects
  "Given a project root, return possible roots
  for other project roots it depends on"
  [project-root]
  (let [props-file (file project-root "project.properties")]
    (when (.exists props-file)
      ; process an android project.properties
      (->> props-file
           (slurp)
           (str/split-lines)
           (filter #(.startsWith % "android.library"))
           (map #(second (str/split % #"=")))
           (map #(file project-root %))))))

(defn detect-source-dirs
  "Given a project root, return possible source dirs"
  [project-root]
  (let [found (->> java-source-dir-candidates
                   (map #(file project-root %))
                   (filter #(.exists %))
                   (java-source-dir-fallback project-root))
        search (detect-depended-projects project-root)] 
    (->> found
         (concat (map detect-source-dirs search)) ; recurse
         (flatten)
         (distinct)
         (map str))))

(defn detect-environment
  "Detect the classpath and sourcepath
  for a given file"
  [path]
  (let [fpath (file path)
        unit (.getName fpath)
        root (detect-project-root fpath)
        srcs (detect-source-dirs root)]
    [(concat (detect-java-core) (detect-gradle-jars root)) ; TODO add other jars
     (map #(str % File/separator) srcs)
     (str/join (drop-last 5 unit))
     root]))

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
        [cp sp unit root] (detect-environment path)
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

  )
