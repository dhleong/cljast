(ns app.gradle-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [app.gradle :refer :all]))

(def gradle-version "1.11")

(deftest gradle-project-test
  (testing "classpath"
    (is (nil? (get-dependencies
                (gradle-project gradle-proj-dir ))))))

;; (clojure.test/run-tests)
