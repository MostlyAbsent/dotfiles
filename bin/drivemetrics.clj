#!/usr/bin/env bb

(require '[clojure.tools.cli :refer [parse-opts]]
         '[clojure.java.shell :refer [sh]])

(def cli-options
  "Destructuring vector for cli options"
  [["-f" "--fusion FUSION"]
   ["-d" "--drive DRIVE"]
   ["-c" "--capacity CAPACITY"]
   ["-t" "--data DATA"]
   ["-o" "--os OS" :parse-fn #(:out (sh "macos" %))]
   ["-a" "--age AGE" :parse-fn #(Integer/parseInt %)]])

(def opts (:options (parse-opts *command-line-args* cli-options)))

(def str-fusion
  (str "Confirmed the machine is using Fusion, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) "."))

(def str-fusion-age
  (str "Confirmed the machine is using Fusion, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) ", "
       "approximately " (:age opts) " years old."))

(def str-no-fusion-age
  (str "Confirmed the machine is not using Fusion, "
       "has a " (:drive opts) " drive, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) ", "
       "approximately " (:age opts) " years old."))

(def str-no-fusion
  (str "Confirmed the machine is not using Fusion, "
       "has a " (:drive opts) " drive, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) "."))

(def str-standard-drive-age
  (str "Confirmed the machine has a " (:drive opts) " drive, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) ", "
       "approximately " (:age opts) " years old."))

(def str-standard-drive
  (str "Confirmed the machine has a " (:drive opts) " drive, "
       "its total capacity is " (:capacity opts) " GB, "
       "with about " (:data opts) " GB of data, "
       "running " (:os opts) "."))

(cond (= (:fusion opts) "TRUE")
  (do (cond (= 0 (:age opts)) (println str-fusion)
            (< 0 (:age opts)) (println str-fusion-age)))
  (= (:fusion opts) "FALSE")
  (do (cond (= 0 (:age opts)) (println str-no-fusion)
            (< 0 (:age opts)) (println str-no-fusion-age)))
  :else
  (do (cond (= 0 (:age opts)) (println str-standard-drive)
            (< 0 (:age opts)) (println str-standard-drive-age))))
