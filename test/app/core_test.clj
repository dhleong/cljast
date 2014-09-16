(ns app.core-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [app.core :refer :all]))

(def foo-java "test/app/src/net/dhleong/test/Foo.java")
(def magic-java "test/app/src/net/dhleong/test/Magic.java")
(def bar-java "test/app/src/net/dhleong/test/bars/Bar.java")

(deftest core-test
  (testing "detect-source-dirs"
    (is (= ["test/app/src"] (detect-source-dirs "test/app"))))

  (testing "detect-environment"
    (let [[cp sp unit root] (detect-environment foo-java)
          expected "test/app/src/"]
      (is (not-empty cp))
      (is (= (apply str (take-last (.length expected) (first sp))) expected))
      (is (= unit "Foo"))
      (is (= (apply str (take-last 8 root)) "test/app"))
      ))

  (testing "read-ast works"
    (let [ast (read-ast foo-java)]
      (is ast))))

(deftest get-errors-test
  (testing "Magic.java has no errors"
    (is (empty? (get-errors (read-ast magic-java)))))
  (testing "Foo.java has errors (for suggestions!)"
    (is (seq (get-errors (read-ast foo-java))))))

(deftest find-node-test
  (let [foo (read-ast foo-java)]
    (testing "find-node 8 16 -> Magic#get() -> Magic"
      (let [node (find-node foo 8 16)]
        (is (not (nil? node)))
        (is (= ASTNode/EXPRESSION_STATEMENT (.getNodeType node)))
        (is (= "net.dhleong.test.Magic" 
               (-> node .getExpression .resolveTypeBinding .getQualifiedName)))))
    ))

(deftest identify-test
  (let [foo (read-ast foo-java)]
    (testing "Instance variable @ 8:8"
      (is (= (identify foo 8 8)
             {:what type-var
              :type "net.dhleong.test.Magic"
              :name "m"
              :javadoc nil
              })))

    (testing "Method invocation @ 8:10"
      (is (= (identify foo 8 10)
             {:what type-method
              :type "net.dhleong.test.Magic"
              :returns "net.dhleong.test.Magic"
              :name "get"
              :args []
              :javadoc "Get some magic"
              })))

    (testing "Static method invocation @ 25:19 (for javadoc, args)"
      (is (= (identify foo 25 19)
             {:what type-method
              :type "net.dhleong.test.Magic"
              :returns "net.dhleong.test.Magic"
              :name "newInstance"
              :args ["java.lang.String"]
              :javadoc "Create Magic\n@return a new piece of Magic"
              })))

    (testing "Static field at 31 27"
      (is (= (identify foo 31 27)
             {:what type-var
              :type "int"
              :name "MAGIC_NUMBER"
              :javadoc "So full of magic"
              ; TODO const value?
              })))
    
    ; TODO class literal
    ; TODO constructor
    ))

(deftest get-suggestions-test
  (let [foo (read-ast foo-java)]
    (testing "suggestions 17 9: simple")
    ; TODO
    (testing "suggestions 12 30: constructor+method-chained")
    ; TODO
    (testing "suggestions 8 16: method-chained")
    ; TODO
    (testing "suggestions 21 13: static fields/methods"
      ; TODO
      ;; (let [s (get-suggestions foo 21 13)]
      ;;   (is (not-empty s))))
)
    ; TODO type suggestions, IE: `new M` -> `new Magic`
    ;       ideally with constructor overloads (this should be easiest part)
    ))

(deftest get-missing-imports-test
  (let [foo (read-ast foo-java)]
    (testing "ArrayList is missing"
      (is (= ["ArrayList"] (get-missing-imports foo))))))

(deftest get-unused-imports-test
  (let [bar (read-ast bar-java)]
    (testing "LinkedList is unused")
      (is (= ["java.util.LinkedList"] (get-unused-imports bar)))))

(clojure.test/run-tests)
