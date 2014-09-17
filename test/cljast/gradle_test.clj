(ns cljast.gradle-test
  (import (org.eclipse.jdt.core.dom AST ASTNode))
  (:require [clojure.test :refer :all]
            [cljast.gradle :as gradle]
            [clojure.java.io :refer [file]]
            ))

(def gradle-version "1.11")
(def gradle-proj-dir "test/cljast/gradle")
(def android-proj-dir "test/cljast/gradle-android")

(deftest gradle-project-test
  (testing "classpath"
    (is (not-empty (->> gradle-proj-dir
                        (gradle/project)
                        (gradle/get-dependencies)
                        )))))

(when (System/getenv "ANDROID_HOME")
  (deftest android-project-test
    (testing "classpath"
      (let [cp (gradle/classpath android-proj-dir)]
        (is (not-empty cp))
        (is (.exists (file (first cp))))
        ))))

;; (clojure.test/run-tests)
