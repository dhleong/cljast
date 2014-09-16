(ns app.gradle-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [app.gradle :refer :all]))

(def gradle-version "1.11")
;; (def gradle-proj-dir "/Users/dhleong/git/ape-minus/")

(deftest gradle-project-test
  (testing "classpath"
    (is (nil? (.getClasspath
                (gradle-project :version gradle-version
                                :project gradle-proj-dir ))))))

;; (clojure.test/run-tests)
