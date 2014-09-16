(ns app.gradle-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [app.gradle :refer :all]
            [clojure.java.io :refer [file]]
            ))

(def gradle-version "1.11")
(def gradle-proj-dir "test/app/gradle")
(def android-proj-dir "test/app/gradle-android")

(deftest gradle-project-test
  (testing "classpath"
    (is (not-empty (->> gradle-proj-dir
                        (gradle-project)
                        (get-dependencies)
                        )))))

;; (deftest android-project-test
;;   (testing "classpath"
;;     (is (nil? (->> android-proj-dir
;;                         (gradle-project)
;;                         (get-dependencies)
;;                         )))))

;; (clojure.test/run-tests)
