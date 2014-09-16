(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
          (java.io ByteArrayOutputStream) 
          (java.net URI))
  (:require [clojure.string :as str]
            [clojure.java.io :refer [file]]))

(defn- gradle-connection
  [project & {:keys [version installation home distro]}]
  {:pre [(not (nil? project))
         (.exists (file project))]}
  (let [conn (.. GradleConnector
                 (newConnector)
                 (forProjectDirectory (file project)))]
    (when installation
      (.useInstallation conn (file installation)))
    (when version
      (.useGradleVersion conn version))
    (when distro
      (.useDistribution conn (URI. distro)))
    (when home
      (.useGradleUserHomeDir conn (file home)))
    (.connect conn)))

(defmacro with-gradle
  "Open a gradle project connection, binding to proj-conn
  by default (you can add `:as name` to your args to customize),
  execute the statements, and close the connection when done"
  [gradle-args & body]
  (let [args-map (if (seq? gradle-args) 
                   (apply assoc {} (drop 1 gradle-args))
                   {})
        var-name (get args-map :as 'proj-conn)]
    `(let [~var-name (apply gradle-connection ~gradle-args)]
       (try
         ~(cons 'do body)
         (finally (.close ~var-name))))))

(defn gradle-project
  "Load an EclipseProject instance from Gradle"
  [& args]
  (with-gradle args
    (.getModel proj-conn EclipseProject)))

(defn gradle-list-dependencies
  "Wacky way of listing dependencies by parsing the 
  'dependencies' task output. Does not include projects,
  but *does* include transitive dependencies from them"
  [& args]
  (let [out (ByteArrayOutputStream.)] 
    (with-gradle args
      (.. proj-conn
          (newBuild)
          (forTasks (into-array ["dependencies"]))
          (setStandardOutput out)
          (run)
          ))
    (->> (.toString out)
         (#(str/split % #"\n\n")) ; wacky; split by double newline
         (filter #(.startsWith % "compile")) ; only compile deps
         (first)                            ; first (only?) string
         (#(str/split % #"\n"))             ; split by lines
         (drop 1)                           ; drop header line
         (filter #(= -1 (.indexOf % "- project "))) ; no projects, please
         (map (fn [raw] 
                (->> raw 
                    (#(str/split % #"--- ")) ; split of human-
                    (second)                 ; -friendly grabage
                    (#(str/split % #":"))    ; separate ID
                    ((fn [[group artifact version]] ; destructure
                       {:group group                ; ... and map
                        :artifact artifact
                        :version (last (str/split version #" -> "))
                        }))))))))
 

(defn get-dependencies 
  "List dependency files for an EclipseProject"
  [proj]
  (map #(.getFile %) (.getClasspath proj)))
