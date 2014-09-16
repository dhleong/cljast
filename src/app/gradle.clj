(ns app.gradle
  (import (org.gradle.tooling GradleConnector)
          (org.gradle.tooling.model GradleProject)
          (org.gradle.tooling.model.eclipse EclipseProject)
          (org.gradle.tooling.model.idea IdeaProject)
          (java.io ByteArrayOutputStream File)
          (java.net URI))
  (:require [clojure.string :as str]
            [clojure.java.io :refer [file]]))

(def gradle-cp-task "\ntask _cljast_cp << { println configurations.compile.asPath }")
(def gradle-cp-task-name "_cljast_cp")

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

(defn gradle-classpath
  "Moar wackiness. Backup the default build.gradle and
  append an extra task that dumps the classpath, then
  parse the output of that task."
  [& args]
  (let [root-dir (first args)
        root-file (file root-dir "build.gradle")
        root-text (slurp root-file)
        bak-file (file (str root-file ".bak"))
        raw-bytes (ByteArrayOutputStream.)] ; get ready
    ; insert our task into the gradle file (yuck, but
    ; it doesn't parse settings correctly if we specify
    ; an alternative build file...)
    (when (= -1 (.lastIndexOf root-text gradle-cp-task-name))
      (spit bak-file root-text) ;; save a backup
      (spit root-file gradle-cp-task :append true)) ; write the task
    ; now, process!
    (with-gradle args
      (.. proj-conn
          (newBuild)
          (withArguments (into-array ["-q"])) ; quiet, please
          (forTasks (into-array [gradle-cp-task-name]))
          (setStandardOutput raw-bytes)
          (run)
          ))
    ; restore the original file
    (-> bak-file (.renameTo root-file))
    ; clean up the output
    (-> (.toString raw-bytes)
        (.trim)
        (str/split (re-pattern File/pathSeparator)))
    ))
 
(defn gradle-subprojects
  [& args]
  (map
    #(.getProjectDirectory %)
    (.getChildren (apply gradle-project args))))

(defn get-dependencies 
  "List dependency files for an EclipseProject"
  [proj]
  (map #(.getFile %) (.getClasspath proj)))
