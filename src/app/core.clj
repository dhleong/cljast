(ns app.core
  (import (org.eclipse.jdt.core.dom ASTParser AST ASTNode)))

(defn create-ast
  "Create AST from a string"
  [s]
  (let [parser (ASTParser/newParser(AST/JLS4))]
    (doto parser
      (.setSource (.toCharArray s))
      (.setStatementsRecovery true)
      (.setResolveBindings true))
    (.createAST parser nil)
    ))

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
   (.accept foo visitor)
   @deepest))

(def suggest-handlers
 {ASTNode/FIELD_ACCESS
  (fn [node]
    (-> node .getExpression .resolveTypeBinding)
    )
  
  } )

  (get-suggestions foo 5 13 )

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

(defn get-errors
  "Get array of error message info from ast"
  [ast]
  (map (fn [obj]
         {:text (.getMessage obj)
          :start (.getStartPosition obj)
          :length (.getLength obj)
          }) 
       (.getMessages ast)))

(defn wrapper
  "wrapper"
  []

  (def foo (create-ast 
             (apply str 
                    (interpose "\r" 
                               ["class Foo {"
                                "  void bar() {"
                                "    Magic m = new Magic();"
                                "    int i = 1;"
                                "    m.get().fo"
                                "  }"
                                "  void foo() {"
                                "     //nop"
                                "  }"
                                "}"]))))

  (.getName (.getParent (find-node foo 5, 13)))

  (.statements (.getBody (first (.getMethods (first (.types foo))))))

  (map #(println (.getLineNumber foo (.getStartPosition %)) (.getMessage %)) (.getMessages foo))

  ;; (org.eclipse.jdt.core.JavaCore/getOptions)

  )
