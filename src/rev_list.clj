(ns rev-list
  (:require [cat-file :as cf]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn allContent [addy]
  (reduce str (-> addy slurp (str/split #" ") second butlast)))

(defn databaseMissing? [dir database]
  (-> (io/file dir database) .isDirectory not))

(defn getBytes [addy]
  (->> addy cf/unzip (cf/split-at-byte (byte 0x00)) second))

(defn committing [addy dir database]
  (->> addy (cf/filePath dir database) getBytes (map char) (apply str)))

(defn commitAddy [dir database headAddy]
  (loop [allList []
         targetAddy headAddy]
    (let [contents (committing targetAddy dir database)
          secondly (-> contents (str/split #"\n") second (str/split #" "))]
      (if (-> secondly first (= "parent"))
        (recur (conj allList targetAddy) (second secondly))
        (conj allList targetAddy)))))

(defn chainRef [dir database func head]
  (->> head (str dir database "/refs/heads/") slurp butlast (reduce str) (func dir database)))

(defn flagHandle [num head dir database]
  (cond
    (nil? num) (println "Error: you must specify a numeric count with '-n'.")
    (->> num (re-matches #"[0-9]+") nil?) (println "Error: the argument for '-n' must be a non-negative integer.")
    (or (nil? head) (= "@" head) (= "HEAD" head)) (flagHandle num (-> (str dir database "/HEAD") allContent (str/split #"/") last) dir database)
    :else (try (->> head (chainRef dir database commitAddy) (take (Integer/parseInt num)) (map #(str % "\n")) (reduce str "") print)
               (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e))))

(defn rev-list [args dir database]
  (let [[head & rest] args]
    (cond
      (or (= head "-h") (= head "--help")) (println "idiot rev-list: list preceding revisions, latest first\n\nUsage: idiot rev-list [-n <count>] [<ref>]\n\nArguments:\n   -n <count>   stop after <count> revisions (default: don't stop)\n   <ref>        a reference; see the rev-parse command (default: HEAD)")
      (databaseMissing? dir database) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= head "-n") (flagHandle (first rest) (second rest) dir database)
      (or (nil? head) (= "@" head) (= "HEAD" head)) (rev-list [(-> (str dir database "/HEAD") allContent (str/split #"/") last)] dir database)
      (not (.exists (io/file (str dir database "/refs/heads/" head)))) (->> head (format "Error: could not find ref named %s.") println)
      :else (try (->> head (chainRef dir database commitAddy) (map #(str % "\n")) (reduce str "") print)
                 (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e)))))