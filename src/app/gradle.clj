(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
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

(defn gradle-project
  "Load an EclipseProject instance from Gradle"
  [& args]
  (with-gradle args 
    (.getModel proj-conn EclipseProject)))

(defmacro with-gradle
  "Open a gradle project connection, binding to proj-conn
  by default (you can add `:as name` to your args to customize),
  execute the statements, and close the connection when done"
  [gradle-args & body]
  (let [args-map (apply assoc {} (drop 1 gradle-args))
        var-name (get args-map :as 'proj-conn)
        pass-args (if (contains? args-map :as)
                    (drop-last 2 gradle-args)
                    gradle-args)]
    (list 'let [var-name (list 'apply 'gradle-connection (vec pass-args))]
          (list 'try
            (cons 'do body)
            (list 'finally (list '.close var-name))))))

(defn get-dependencies 
  "List dependency files for an EclipseProject"
  [proj]
  (map #(.getFile %) (.getClasspath proj)))
