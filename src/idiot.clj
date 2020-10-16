(ns idiot
  (:require
   [write-tree :as write-tree]
   [hash-object :as hash-object]
   [cat-file :as  cat-file]
   [commit-tree :as commit-tree]
   [init :as init]
   [r-switch :as rs]
   [d-switch :as ds]
   [rev-parse]
   [rev-list]
   [switch]
   [branch]
   [commit]
   [log]
   [explore]))

(defn help [params]
  (cond
    ;no other things but help
    (= 0 (count params))
    (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   explore [-p <port>]\n   hash-object [-w] <file>\n   help\n   init\n   log --oneline [-n <count>] [<ref>]\n   rev-list [-n <count>] [<ref>]\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
    ;if it's help
    (or (= (first params) "-h") (= (first params) "--help") (= (first params) "help"))
    (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   explore [-p <port>]\n   hash-object [-w] <file>\n   help\n   init\n   log --oneline [-n <count>] [<ref>]\n   rev-list [-n <count>] [<ref>]\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
    ;help init message
    (= (first params) "init")
    (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    ;help hash
    (= (first params) "hash-object")
    (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    ;help cat
    (= (first params) "cat-file")
    (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
    ;write tree
    (= (first params) "write-wtree")
    (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
    ; commit tree
    (= (first params) "commit-tree")
    (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
    ;rev-parse
    (= (first params) "rev-parse")
    (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
    ;switch
    (= (first params) "switch")
    (println "idiot switch: change what HEAD points to\n\nUsage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it")
    ;branch
    (= (first params) "branch")
    (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
    ;commit
    (= (first params) "commit")
    (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
    ;rev-list
    (= (first params) "rev-list")
    (println "idiot rev-list: list preceding revisions, latest first\n\nUsage: idiot rev-list [-n <count>] [<ref>]\n\nArguments:\n   -n <count>   stop after <count> revisions (default: don't stop)\n   <ref>        a reference; see the rev-parse command (default: HEAD)")
    ;log oneline
    (= (first params) "log --oneline")
    (println "idiot log: print abbreviated commit addresses and commit summaries\n\nUsage: idiot log --oneline [-n <count>] [<ref>]\n\nArguments:\n   -n <count>   stop after <count> revisions (default: don't stop)\n   <ref>        a reference; see the rev-parse command (default: HEAD)\n")
    ;invalid command
    (or (not= (count params) 0) (not= (first params)  ""))
    (println "Error: invalid command")))

(defn send2fxn [mArgs dir database]
  (let [command (first mArgs)]
    (cond
      ;eval directory change
      (= "-r" command) (rs/run-rSwitch (rest mArgs) database)
      ;eval database change
      (= "-d" command) (ds/d-switch (rest mArgs) dir)
      ;fxns adjusted for A3
      (= command "help") (help (rest mArgs))
      (= command "write-wtree") (write-tree/write-tree (rest mArgs) dir database)
      (= command "init") (init/init (rest mArgs) dir database)
      (= command "hash-object") (hash-object/hash-object (rest mArgs) dir database)
      (= command "cat-file") (cat-file/cat-file (rest mArgs) dir database)
      (= command "commit-tree") (commit-tree/commit-tree (rest mArgs) dir database)
      (= command "rev-parse") (rev-parse/rev-parse (rest mArgs) dir database)
      (= command "switch") (switch/switch (rest mArgs) dir database)
      (= command "branch") (branch/branch (rest mArgs) dir database)
      ;not implemented yet

      ;(= command "switch") (switch/switch (rest mArgs) dir database)
      (= command "commit") (commit/commit (rest mArgs) dir database)
      (= command "rev-list") (rev-list/rev-list (rest mArgs) dir database)
      (= command "log") (log/log (rest mArgs) dir database)
      (= command "explore") (explore/explore (rest mArgs) dir database)

      ;help messages Again
      ;(or(= command "-h") (= command "--help") (= command " ") (= mArgs nil)) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file {-p|-t} <address>\n   write-wtree\n   commit-tree <tree> -m \"<message>\" [(-p <parent>)...]")

      :else
      (print "Error: invalid command\n"))))

; too many issues needed for r switch and db, main will
; now call to helper that will splits main args -> mArgs
;will also send default repo as ./
;default database as ".idiot"  --> not too sure for this
; just assume only exists in idiot for top level dir
;helper function will check if need to overwrite repo and database for r/d switch
(defn -main [& args]
  (cond
    (or (= (first args) "-h") (= (first args) "--help") (= (first args) " ") (= args nil)) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   explore [-p <port>]\n   hash-object [-w] <file>\n   help\n   init\n   log --oneline [-n <count>] [<ref>]\n   rev-list [-n <count>] [<ref>]\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
    (not (or (= (first args) "-h") (= (first args) "--help") (= (first args) " ") (= args nil)))
    (send2fxn args "./" ".idiot")))