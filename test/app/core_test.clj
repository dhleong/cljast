(ns app.core-test
  (:require [clojure.test :refer :all]
            [app.core :refer :all]))

(def foo-java "test/app/src/net/dhleong/test/Foo.java")

(deftest basic-test
  (testing "detect-environment"
    (let [[cp sp unit] (detect-environment foo-java)]
      (is (.endsWith (first sp) "test/app/src"))))

  (testing "read-ast"
    (let [ast (read-ast foo-java)]
      (is ast))))
