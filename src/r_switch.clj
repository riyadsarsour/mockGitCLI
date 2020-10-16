(ns r-switch
  (:require [clojure.java.io :as io]
            [write-tree :as write-tree]
            [hash-object :as hash-object]
            [cat-file :as  cat-file]
            [commit-tree :as commit-tree]
            [init :as init]
            [rev-parse :as rev-parse]
            [rev-list]
            [switch]
            [branch]
            [commit]
            [log]
            [explore]))

(defn sendFxn [mArgs dir database]
  (cond
    (= (first mArgs) "init") (init/init (rest mArgs) dir database)
    (= (first mArgs) "hash-object") (hash-object/hash-object (rest mArgs) dir database)
    (= (first mArgs) "write-wtree") (write-tree/write-tree   (rest mArgs) dir database)
    (= (first mArgs) "commit-tree") (commit-tree/commit-tree (rest mArgs) dir database)
    (= (first mArgs) "cat-file") (cat-file/cat-file (rest mArgs) dir database)
    (= (first mArgs) "rev-parse") (rev-parse/rev-parse (rest mArgs) dir database)
    (= (first mArgs) "switch") (switch/switch (rest mArgs) dir database)
    (= (first mArgs) "branch") (branch/branch (rest mArgs) dir database)
    (= (first mArgs) "commit") (commit/commit (rest mArgs) dir database)
    (= (first mArgs) "rev-list") (rev-list/rev-list (rest mArgs) dir database)
    (= (first mArgs) "log") (log/log (rest mArgs) dir database)
    (= (first mArgs) "explore") (explore/explore (rest mArgs) dir database)
    ;help messages Again
    ;(or(= (first args) "-h") (= (first args) "--help") (= (first args) " ") (= args nil)) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file {-p|-t} <address>\n   write-wtree\n   commit-tree <tree> -m \"<message>\" [(-p <parent>)...]")
    :else
    (print "Error: invalid command\n")))

(defn d-switch [args dir]
  (let [database (first args)]
    (cond
      (= (count database) 0) (println "Error: the -d switch needs an argument")
      :else
      (sendFxn (rest args) dir database))))

(defn run-rSwitch [args database]
  (let [workDir (first args)
        command+args (rest args)
        command (first command+args)
        otherArgs (rest command+args)]
    (cond
      (= (count workDir) 0) (println "Error: the -r switch needs an argument")
      :else
      (cond
        ;see if the file is NOT in working directory
        (not (.exists  (io/as-file (str workDir)))) (println "Error: the directory specified by -r does not exist")
        ;if d switch there too
        (= "-d" command) (d-switch otherArgs workDir)
        ; last check for not enough args, had issues in autograder but not cli ?
        (nil? workDir) (println "Error: the -r switch needs an argument")

        ; make the switch of directory if no d switch
        :else
        (sendFxn command+args (str workDir "/")  database)))))