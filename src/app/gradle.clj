(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
          (java.io ByteArrayOutputStream) 
          (java.net URI))
  (:require [clojure.java.io :refer [file]]))

(defn- gradle-connection
  [project & {:keys [version installation home distro]}]
  {:pre [(not (nil? project))]}

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
         (do ~body)
         (finally (.close ~var-name))))))

(defn gradle-project
  "Load an EclipseProject instance from Gradle"
  [& args]
  (with-gradle args
    (.getModel proj-conn EclipseProject)))

(defn gradle-list-dependencies
  "Wacky way of listing dependencies"
  [& args]
  (let [out (ByteArrayOutputStream.)] 
    (with-gradle args
      (.. proj-conn
          (newBuild)
          (forTasks (into-array ["dependencies"]))
          (setStandardOutput out)
          (run)
          ))
    (.toString out)))
 

(defn get-dependencies 
  "List dependency files for an EclipseProject"
  [proj]
  (map #(.getFile %) (.getClasspath proj)))
