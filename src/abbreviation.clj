(ns abbreviation
  (:require [clojure.java.io :as io]))

(defn search [addy dir database]
  (try (let [files (->> (subs addy 0 2)
                        (str dir database "/objects/")
                        io/file
                        .listFiles
                        (map #(.getName %)))
             initial-chars (subs addy 2)
             chars-len (count initial-chars)
             matching-files (filter #(= initial-chars (subs % 0 chars-len)) files)
             mapped-files (map #(str (subs addy 0 2) %) matching-files)]
         (if (= 0 (count matching-files))
           [0 addy]
           [(count matching-files) mapped-files]))
       (catch Exception e e [0, nil])))

(defn addyError [addy len msg]
  (if (> len 1)
    (->> addy (format "Error: ambiguous match for address '%s'") println)
    (println msg)))

(defn addyErrorCommit [addy len msg]
  (if (> len 1)
    (format "Error: ambiguous match for address '%s'" addy)
    (print msg)))