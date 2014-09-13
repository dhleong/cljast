(ns app.core
  (import (org.eclipse.jdt.core.dom ASTParser AST ASTNode Modifier)
          (java.io File))
  (:require [clojure.string :as str]))

(def type-method "m")
(def type-var "v")

(def osx-java-core "/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar")

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
        pathDir "test/app/src"
        unit (last parts)
        absPath (.getAbsolutePath (File. "test/app/src"))]
    [(concat (detect-java-core)) ; TODO add other jars
     [absPath]                   ; boring
     (str/join (drop-last 5 unit))]))

(defn create-ast
  "Create AST from args:
  :text Text contents of file
  :path Path to the file
  "
  [& {:keys [text path]}]
  (let [parser (ASTParser/newParser(AST/JLS4))
        [cp sp unit] (detect-environment path)
        acp (into-array java.lang.String cp)
        asp (into-array java.lang.String sp)]
    (doto parser
      (.setSource (.toCharArray text))
      (.setStatementsRecovery true)
      (.setEnvironment acp asp nil false)
      (.setUnitName unit)
      (.setResolveBindings true))
    (.createAST parser nil)
    ))

(defn read-ast
  "Read an AST from a file"
  [path]
  (when-let [text (slurp path)]
    (create-ast :text text :path path)))

(defn find-node
  "Search for an expression in the AST
  at the given line, col"
  [ast line col]
  (let [pos (.getPosition ast line col)
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
  [ast line col]
  (let [node (find-node ast line col)]
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
  [ast line col]
  (when-let [node (find-node ast line col)]
    (handle-suggestion node)))

(defn identify
  "Get information about the AST node at a given position"
  [ast line col]
  (when-let [node (find-node ast line col) ]
    ; TODO var? method decl? class literal?
     {:what type-var
      :name (.getIdentifier node)
      :type (-> node .resolveTypeBinding .getQualifiedName)
      }))

(defn get-errors
  "Get array of error message info from ast"
  [ast]
  (map (fn [obj]
         (let [start-pos (.getStartPosition obj)]
           {:text (.getMessage obj)
            :pos start-pos
            :line (.getLineNumber ast start-pos)
            :col (.getColumnNumber ast start-pos)
            :length (.getLength obj)
            })) 
       (.getMessages ast)))

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
