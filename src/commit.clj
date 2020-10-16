(ns commit
  (:require [clojure.java.io :as io])
  (:require [commit-tree :as ct]
            [cat-file :as cf]
            [branch]
            [rev-parse :as rp]))

(defn committing [addy dir database]
  (println "Commit created.")
  (let [databasePath (str dir "/" database)
        name (branch/branchName dir database)
        headPath (str databasePath "/" "HEAD")
        is-ref (-> headPath slurp rp/is-ref?)
        refAddy (str databasePath "/refs/heads/" name)]
    (when is-ref (spit refAddy (str addy "\n"))
          (println (format "Updated branch %s." name)))))

(defn commit [args dir database]
  (let [[addy flag message & details] args
        addyPair (partition 2 details)
        allAddy (map second addyPair)
        databasePath (str dir "/" database)]
    (cond (or (= addy "-h") (= addy "--help")) (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
          (not (.exists (io/file databasePath))) (println "Error: could not find database. (Did you run `idiot init`?)")
          (or (nil? addy) (= flag addy)) (println "Error: you must specify a tree address.")
          (not (.exists (io/file (ct/getAddy addy dir database)))) (println "Error: no tree object exists at that address.")
          (not= (cf/objectType? dir database addy) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
          (not= flag "-m") (println "Error: you must specify a message.")
          (nil? message) (println "Error: you must specify a message with the -m switch.")
          (odd? (count details)) (println "Error: you must specify a commit object with the -p switch.")
          :else (if (not (nil? details))
                  (let [results (ct/try2commit addyPair dir database)]
                    (if results
                      (println results)
                      (let [commitAddy (->> allAddy
                                            (map #(str "parent " % "\n"))
                                            (reduce str)
                                            (ct/commit-object addy ct/author ct/committer message)
                                            (ct/create-commit dir database))]
                        (committing commitAddy dir database))))
                  (let [commitAddy2 (->> (ct/commit-object addy ct/author ct/committer message "")
                                         (ct/create-commit dir database))]
                    (committing commitAddy2 dir database))))))
;(ns commit
;  (:require [clojure.java.io :as io])
;  (:require [commit-tree :as ct]
;            [cat-file :as cf]
;            [branch]
;            [rev-parse :as rp]
;            [abbreviation :as ab]))
;
;;(defn committing [addy dir database]
;;  (println "Commit created.")
;;  (let [databasePath (str dir "/" database)
;;        name (branch/branchName dir database)
;;        headPath (str databasePath "/" "HEAD")
;;        is-ref (-> headPath slurp rp/is-ref?)
;;        refAddy (str databasePath "/refs/heads/" name)]
;;    (when is-ref (spit refAddy (str addy "\n"))
;;          (println (format "Updated branch %s." name)))))
;
;(defn committing [addy dir database]
;  (let [databasePath (str dir "/" database)
;        name (branch/branchName dir database)
;        refAddy (str databasePath "/refs/heads/" name)
;        spit #(spit refAddy (str % "\n"))]
;    (when (not (= nil addy))
;      (println "Commit created.")
;      (when (rp/is-ref? (str dir database "/HEAD"))
;        (spit addy)
;        (println (str "Updated branch " name "."))))))
;
;(defn commit [args dir database]
;  (let [[addy flag message & details] args
;        info (ab/search addy dir database)
;        matching (first info)
;        newAddy (first (second info))
;        databasePath (str dir "/" database)]
;    (cond (or (= addy "-h") (= addy "--help")) (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
;          (not (.exists (io/file databasePath))) (println "Error: could not find database. (Did you run `idiot init`?)")
;          (or (= nil addy) (= flag addy)) (println "Error: you must specify a tree address.")
;          (not= 1 matching) (ab/addyError addy matching "Error: no tree object exists at that address.")
;          (not= (cf/objectType? dir database addy) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
;          (not= flag "-m") (println "Error: you must specify a message.")
;          (nil? message) (println "Error: you must specify a message with the -m switch.")
;          (odd? (count details)) (println "Error: you must specify a commit object with the -p switch.")
;          :else (committing (ct/try2commit message newAddy details dir database) dir database))))