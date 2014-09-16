(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
          (java.net URI))
  (:require [clojure.java.io :refer [file]]))

(defn- gradle-connection
  [& {:keys [project version installation home distro]}]
  {:pre [(count project)]}

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
  (let [proj-conn (apply gradle-connection args)]
    (try
      (.getModel proj-conn EclipseProject)
      (finally (.close proj-conn))
      )))
