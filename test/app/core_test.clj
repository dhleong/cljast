(ns app.core-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [app.core :refer :all]))

(def foo-java "test/app/src/net/dhleong/test/Foo.java")
(def magic-java "test/app/src/net/dhleong/test/Magic.java")

(deftest core-test
  (testing "detect-environment"
    (let [[cp sp unit] (detect-environment foo-java)]
      (is (.endsWith (first sp) "test/app/src"))
      (is (not-empty cp))
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
    ; TODO method invocation with args
    ; TODO static field
    ; TODO class literal
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


;; (clojure.test/run-tests)
