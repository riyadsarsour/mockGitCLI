(ns init
  (:require
   [clojure.java.io :as io]))

; A2 Init
;(defn init [arg] (
;                  (cond
;                    ;help message
;                    (or (= (first arg) "-h") (= (first arg) "--help") )
;                    (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
;
;                    ;invalid init command
;                    (and (not= (count arg) 0) (or (not= arg "-h") (not= arg "--help") ) ) (println "Error: init accepts no arguments")
;                    ;existing direc
;                    (.exists (io/file ".git")) (println "Error: .git directory already exists")
;                    :else (do (.mkdir (io/file ".git")) (.mkdir (io/file ".git/objects"))
;                              (println "Initialized empty Idiot repository in .git directory"))
;                    )
;                  ))

;A3 rewrite of Init
(defn init [args direc database]
  (cond
    (and (= (count args) 1) (or (= (first args) "-h") (= (first args) "--help")))
    (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    (and (not= (count args) 0) (or (not= args "-h") (not= args "--help"))) (println "Error: init accepts no arguments")
    ;existing directory
    (.isDirectory (io/file direc database)) (println (format "Error: %s directory already exists" database))
    :else
    ; init  new directory
    (do (io/make-parents (str direc "/" database "/objects/sample.txt"))
        (io/make-parents (str direc "/" database "/refs/heads/sample.txt"))
        (spit (str direc "/" database "/HEAD") "ref: refs/heads/master\n")
        (println (format "Initialized empty Idiot repository in %s directory" database)))))

;(do
;  (io/make-parents (str direc "/" database "/objects/sample.txt"))
;  (println (format "Initialized empty Idiot repository in %s directory" database))