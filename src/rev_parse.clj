(ns rev-parse
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn is-ref? [head]
  (str/starts-with? head "ref:"))

(defn ifHead [head dirDB]
  (let [ref (-> head (str/split #" ") second str/trim-newline)
        refPath (str dirDB "/" ref)]
    (when (.exists (io/file refPath))
      (slurp refPath))))

(defn commitFinder [dir database]
  (let [dirDB (str dir "/" database)
        headPath (str dirDB "/" "HEAD")
        head (slurp headPath)]
    (cond
      (is-ref? head) (ifHead head dirDB)
      :else
      head)))

(defn rev-parse [args dir database]
  (let [cmd (first args)
        dirDb (str dir "/" database)
        refPath (str dirDb "/refs/heads/" cmd)]
    (cond
      ;handle help cmd
      (or (= cmd "-h") (= cmd "--help"))
      (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
      ;check if no cmd
      (nil? cmd) (println "Error: you must specify a branch name.")
      ;too may args w branch
      (> (count args) 1) (println "Error: you must specify a branch name and nothing else.")
      ;see if it exists
      (not (.exists (io/file dirDb))) (println "Error: could not find database. (Did you run `idiot init`?)")
      ;send head to find commit
      (or (= cmd "HEAD") (= cmd "@")) (print (commitFinder dir database))
      ;if ref cannot be found
      (not (.exists (io/file refPath))) (println (format "Error: could not find ref named %s." cmd))
      ;else print slurp info in path
      :else
      (print (slurp refPath)))))