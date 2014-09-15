(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
          (java.io File)))

(defn wrapper []

  (def ape-minus (.. GradleConnector 
                     (newConnector)
                     (forProjectDirectory (File. "/Users/dhleong/git/ape-minus/"))
                     (connect)))

  (def ape-minus-proj (.getModel ape-minus GradleProject))
  (def ape-minus-eclipse (.getModel ape-minus EclipseProject))
  
(let [
      kids (-> ape-minus-proj .getChildren)
      ]
  (println (-> ape-minus-proj .getBuildScript .getSourceFile))
  (map #(-> % .getBuildScript .getSourceFile) kids))

  (map identity (.getClasspath ape-minus-eclipse))

)
