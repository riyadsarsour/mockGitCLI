(ns commit-tree
  (:require [clojure.java.io :as io]
            [sha :as sha]
            [byte-array :as ba]
            [cat-file :as cat]))

(def author "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500")
(def committer author)

(defn getAddy [addy dir database]
  (str dir "/" database "/objects/" (subs addy 0 2) "/" (subs addy 2)))

(defn getBytes [commit]
  (-> commit .getBytes sha/sha-bytes))

(defn buildObject [dir database addy contents]
  (let [path (getAddy addy dir database)]
    (cond (not (.exists (io/file path))) (do (io/make-parents path)
                                             (io/copy (sha/zipStr contents)
                                                      (io/file path))))))

(defn commit-object [tree-addr author-str committer-str msg parent-str]
  (let [commit-format (str "tree %s\n"
                           "%s" "author %s\n"
                           "committer %s\n"
                           "\n"
                           "%s\n")
        commit-str (format commit-format
                           tree-addr
                           parent-str
                           author-str
                           committer-str
                           msg)]
    (format "commit %d\000%s"
            (count commit-str)
            commit-str)))

(defn create-commit [dir database commit]
  (let [info-bytes (getBytes commit)
        addy (-> info-bytes ba/to-hex-string)]
    (buildObject dir database addy commit)
    addy))

(defn try2commit [parents dir database]
  (loop [all (seq parents)]
    (if all
      (let [pair (first all)
            flag (first pair)
            addy (second pair)]
        (if (= flag "-p")
          (cond (not (.exists (io/file (getAddy addy dir database)))) (str "Error: no commit object exists at address " addy ".")
                (not= (cat/objectType? dir database addy) "commit") (str "Error: an object exists at address " addy ", but it isn't a commit.")
                :else (recur (next all)))
          (str "Error: invalid command")))
      nil)))

(defn commit-tree [args dir database]
  (let [[addy flag message & details] args
        addyPair (partition 2 details)
        allAddy (map second addyPair)
        databasePath (str dir "/" database)]

    (cond
      ;help usage case
      (or (= addy "-h") (= addy "--help"))
      (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
      ;if not found in database
      (not (.exists (io/file databasePath))) (println "Error: could not find database. (Did you run `idiot init`?)")
      ;no tree address given
      (or (nil? addy) (= flag addy)) (println "Error: you must specify a tree address.")
      ;not an address that exists
      (not (.exists (io/file (getAddy addy dir database)))) (println "Error: no tree object exists at that address.")
      ;object at addy is no tree
      (not= (cat/objectType? dir database addy) "tree") (println "Error: an object exists at that address, but it isn't a tree.")
      ;no message given
      (not= flag "-m") (println "Error: you must specify a message.")
      ;no message specified
      (nil? message) (println "Error: you must specify a message with the -m switch.")
      ;not commit object given with -p switch
      (odd? (count details)) (println "Error: you must specify a commit object with the -p switch.")
      ;go to work print commits
      :else (if (not (nil? details))
              (let [results (try2commit addyPair dir database)]
                (if results
                  (println results)
                  (println (->> allAddy (map #(str "parent " % "\n"))
                                (reduce str) (commit-object addy author committer message)
                                (create-commit dir database)))))
              (println (->> (commit-object addy author committer message "")
                            (create-commit dir database)))))))

;(defn tester [args]
;  (let [one (first args),
;        rem (rest args)
;
;        (cond
;          (nil? rem) one
;          :else rem)]))
;
;(-> '(1 2 3 4 5) tester)

;(defn tester [args]
;  (let [[one two & rest] args]
;    rest))
;
;(tester '(1 2 3 4 5))


;(ns commit-tree
;  (:require [clojure.java.io :as io]
;            [sha :as sha]
;            [byte-array :as ba]
;            [cat-file :as cat]
;            [abbreviation :as ab]
;            [write-tree :as wt]))
;
;(def author "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500")
;(def committer author)
;
;(defn getAddy [addy dir database]
;  (str dir "/" database "/objects/" (subs addy 0 2) "/" (subs addy 2)))
;
;(defn getBytes [commit]
;  (-> commit .getBytes sha/sha-bytes))
;
;(defn firstFalse [func all]
;  (try (let [evaluation (take-while func all)]
;         (if (= (count all) (count evaluation))
;           nil
;           [(->> evaluation count (nth all)) (count evaluation)]))
;       (catch Exception e
;         e
;         nil)))
;
;(defn buildObject [dir database addy contents]
;  (let [path (getAddy addy dir database)]
;    (cond (not (.exists (io/file path))) (do (io/make-parents path)
;                                             (io/copy (sha/zipStr contents)
;                                                      (io/file path))))))
;
;(defn commit-object [parent-str tree-addr author-str committer-str msg]
;  (let [commit-format (str "tree %s\n"
;                           "%s" "author %s\n"
;                           "committer %s\n"
;                           "\n"
;                           "%s\n")
;        commit-str (format commit-format
;                           tree-addr
;                           parent-str
;                           author-str
;                           committer-str
;                           msg)]
;    (format "commit %d\000%s"
;            (count commit-str)
;            commit-str)))
;
;(defn create-commit [dir database commit]
;  (let [info-bytes (getBytes commit)
;        addy (-> info-bytes ba/to-hex-string)]
;    (buildObject dir database addy commit)
;    addy))
;
;(defn try2commit [msg addy parent dir database]
;  (let [pair (partition-all 2 parent)
;        infoAddy (map #(ab/search (second %) dir database) pair)
;        pairLen (second (first (firstFalse #(> (count (second %)) 3) pair)))
;        match (firstFalse #(= (first %) 1) infoAddy)
;        type (firstFalse #(= "commit" (cat/objectType? (first (second %)) dir database)) infoAddy)
;        concat (fn [commit] (reduce str "" (map #(str "parent " % "\n") commit)))]
;    (cond
;      (= 1 (count (last pair))) (println "Error: you must specify a commit object with the -p switch.")
;      (not= nil pairLen) (format "Error: too few characters specified for address '%s'" pairLen)
;      (not (.exists (io/file (getAddy (second pair) dir database)))) (str "Error: no commit object exists at address " (second pair) ".")
;      (not= nil match) (ab/addyErrorCommit (->> (second match) (nth pair) second) (first (first match)) (format "Error: no commit object exists at address %s." (->> (second match) (nth pair) second)))
;      (not= nil type) (println (format "Error: an object exists at address %s, but it isn't a commit." (->> (second type) (nth pair) second)))
;      :else (-> (concat (map #(first (second %)) infoAddy))
;                (commit-object addy author author msg)
;                .getBytes
;                (wt/storeObject dir database)))))
;
;(defn commit-tree [args dir database]
;  (let [[addy flag message & details] args
;        info (ab/search addy dir database)
;        matching (first info)
;        infoAddy (first (second info))]
;    (cond
;      ;help usage case
;      (or (= addy "-h") (= addy "--help")) (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
;      ;database doesn't exist
;      (not (.exists (io/file (str dir "/" database)))) (println "Error: could not find database. (Did you run `idiot init`?)")
;      ;no address given
;      (= nil addy) (println "Error: you must specify a tree address.")
;      ;if the given address isn't at least 4 characters wrong
;      (< (count addy) 4) (format "Error: too few characters specified for address '%s'" addy)
;      ;if there is no matching object
;      (not= 1 matching) (ab/addyErrorCommit addy matching "Error: no tree object exists at that address.\n")
;      ;if the object types aren't tree
;      (not= "tree" (cat/objectType? dir database addy)) (println "Error: an object exists at that address, but it isn't a tree.")
;      ;no message given
;      (not= flag "-m") (println "Error: you must specify a message.")
;      ;no message specified
;      (nil? message) (println "Error: you must specify a message with the -m switch.")
;      :else (let [newAddy (try2commit message infoAddy details dir database)]
;              (when (not= nil newAddy))
;              (println newAddy)))))
;
;;(defn tester [args]
;;  (let [one (first args),
;;        rem (rest args)
;;
;;        (cond
;;          (nil? rem) one
;;          :else rem)]))
;;
;;(-> '(1 2 3 4 5) tester)
;
;;(defn tester [args]
;;  (let [[one two & rest] args]
;;    rest))
;;
;;(tester '(1 2 3 4 5))