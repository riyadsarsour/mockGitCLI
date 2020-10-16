(ns switch
  (:require [clojure.java.io :as io])
  (:require [rev-parse :as rev]))

(defn handleC [branch dir database refPath]
  (let [addy (rev/commitFinder dir database)
        headPath (str dir "/" database "/" "HEAD")]
    (when (not (nil? addy)) (spit refPath addy))
    (spit headPath (format "ref: refs/heads/%s\n" branch))
    (println (format "Switched to a new branch '%s'" branch))))

(defn elseSwitchBranch [cmd dir database]
  (let [headPath (str dir "/" database "/" "HEAD")]
    (spit headPath (format "ref: refs/heads/%s\n" cmd))
    (println (format "Switched to branch '%s'" cmd))))

(defn switch [args dir database]
  (let [cmd (first args)
        dirDB (str dir "/" database)
        branch (second args)
        refPath (str dirDB "/refs/heads/" (if (= cmd "-c") branch cmd))]
    (cond
      ;handle help
      (or (= cmd "-h") (= cmd "--help"))
      (println "idiot switch: change what HEAD points to\n\nUsage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it")
      ;no args
      (= (count args) 0) (println "Error: you must specify a branch name.")
      ;too many args
      (and (> (count args) 1) (not= cmd "-c")) (println "Error: you may only specify one branch name.")
      (and (= cmd "-c") (> (count args) 2)) (println "Error: you may only specify one branch name.")
      ;not found in database
      (not (.exists (io/file dirDB))) (println "Error: could not find database. (Did you run `idiot init`?)")
      ;if the -c switch is present and a ref with the given name already exists,
      (and (= cmd "-c") (.exists (io/file refPath))) (println "Error: a ref with that name already exists.")
      ;if the -c switch is not present and no ref with the given name exists,
      (and (not= cmd "-c") (not (.exists (io/file refPath)))) (println "Error: no ref with that name exists.")
      ;otherwise handle -c
      (= cmd "-c") (handleC branch dir database refPath)
      :else
      (elseSwitchBranch cmd dir database))))