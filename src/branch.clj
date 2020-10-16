(ns branch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;name of the current branch
(defn branchName [dir database]
  (let [dbAddy (str dir "/" database)
        headAddy (str dbAddy "/" "HEAD")]
    (-> headAddy slurp (str/split #"\n") first (str/split #"/") last)))

(defn allBranches [dir database]
  (let [dbAddy (str dir "/" database)
        refsAddy (str dbAddy "/refs/heads")
        branches (->> refsAddy io/file .listFiles (sort-by #(.getName %)))
        current (branchName dir database)]
    (doseq [this branches] (let [name (.getName this)]
                             (if (not= name current) (println (str "  " name))
                                 (println (str "* " name)))))))

(defn branch [args dir database]
  (let [flag (first args)
        name (second args)
        dbAddy (str dir "/" database)
        refAddy (str dbAddy "/refs/heads/" (if (= flag "-d") name flag))]
    (cond (or (= flag "-h") (= flag "--help")) (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
          (and (= flag "-d") (nil? name)) (println "Error: you must specify a branch name.")
          (or (and (not= flag "-d") (not= nil flag)) (< 2 (count args))) (println "Error: invalid arguments.")
          (not (.exists (io/file dbAddy))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (and (= flag "-d") (not (.exists (io/file refAddy)))) (println (format "Error: branch '%s' not found." name))
          (= flag "-d") (cond (= (branchName dir database) name) (println (format "Error: cannot delete checked-out branch '%s'." name))
                              :else (do (io/delete-file refAddy)
                                        (println (format "Deleted branch %s." name))))
          :else (allBranches dir database))))