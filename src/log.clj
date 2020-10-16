(ns log
  (:require [clojure.string :as str]
            [rev-list :as rl]
            [clojure.java.io :as io]))

(defn locateFirst [eval x]
  (loop [arr x y 0]
    (cond
      (eval (first arr)) y
      :else (recur (next arr) (inc y)))))

(defn onelineFormat [addy msg]
  (str (subs addy 0 7) " " msg "\n"))

(defn logChain [dir database headAddy]
  (loop [info-list []
         target headAddy]
    (let [lines (str/split (rl/committing target dir database) #"\n")
          sline (-> lines second (str/split #" "))
          mline (->> lines (map #(first (str/split % #" "))) (locateFirst (partial = "committer")) (+ 2) (nth lines))]
      (cond
        (-> sline first (= "parent")) (recur (conj info-list [target mline]) (second sline))
        :else
        (conj info-list [target mline])))))

(defn handleN [number ref dir database]
  (cond
    (= nil number) (println "Error: you must specify a numeric count with '-n'.")
    (->> number (re-matches #"[0-9]+") nil?) (println "Error: the argument for '-n' must be a non-negative integer.")
    (or (= nil ref) (= "@" ref) (= "HEAD" ref)) (handleN number (-> (str dir database "/HEAD") rl/allContent (str/split #"/") last) dir database)
    (not (.exists (io/file (str dir database "/refs/heads/" ref)))) (->> ref (format "Error: could not find ref named %s.") println)
    :else (try (->> ref
                    (rl/chainRef dir database logChain)
                    (take (Integer/parseInt number))
                    (map (fn [tuple] (onelineFormat (first tuple) (second tuple))))
                    (reduce str "")
                    print)
               (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e))))

(defn log [args dir database]
  (let [[oneline head & rest] args]
    (cond
      ;help message
      (or (= oneline "-h") (= oneline "--help")) (println "idiot log: print abbreviated commit addresses and commit summaries\n\nUsage: idiot log --oneline [-n <count>] [<ref>]\n\nArguments:\n   -n <count>   stop after <count> revisions (default: don't stop)\n   <ref>        a reference; see the rev-parse command (default: HEAD)")
      ; no onle line keyword
      (not (= oneline "--oneline")) (println "Error: log requires the --oneline switch")
      ;check validity of database
      (rl/databaseMissing? dir database) (println "Error: could not find database. (Did you run `idiot init`?)")
      ;check -n and handle
      (= head "-n") (handleN (first rest) (second rest) dir database)
      ;handle lack of n switch but given ref
      (or (= nil head) (= "@" head) (= "HEAD" head)) (log ["--oneline" (-> (str dir database "/HEAD") rl/allContent (str/split #"/") last)] dir database)
      ;ref not found
      (not (.exists (io/file (str dir database "/refs/heads/" head)))) (println (str "Error: could not find ref named " head "."))
      :else (try (->> head (rl/chainRef dir database logChain) (map (fn [tuple] (onelineFormat (first tuple) (second tuple)))) (reduce str "") print)
                 (catch java.io.FileNotFoundException e (println "Error: that address doesn't exist.") e)))))
