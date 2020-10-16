(ns write-tree
  (:require [clojure.java.io :as io]
            [sha :as sha]
    ;[hash-object :as hash]
            [byte-array :as ba]
    ;[clojure.string :as str]
            [hash-object :as hash])
  ;(:import (java.io ByteArrayOutputStream ByteArrayInputStream)
  ;         (java.util.zip DeflaterOutputStream)
  ;         (java.security MessageDigest))
  )

;;build blob entry in a tree
;(defn blobMaker[file dir database]
;  (hash-object/store-blob (subs (.getPath file) (count dir)) dir database)
;  (ba/concat (.getBytes (str "100644 " (.getName file) "\000")) (ba/from-hex-string (hash/sha1-sum (hash/blobNhead file))))
;  )
;
;(defn treeData [entries]
;  (byte-array (concat (.getBytes (str "tree " (count entries) "\000")) entries))
;  )
;
;(defn treeAddy [tree]
;  (sha/sha-bytes tree)
;  )

;same as one in hash but idk why autgrader doesnt like calling it from hash ns
(defn pathMaker [addy dirDB]
  (str dirDB "/objects/" (subs addy 0 2) "/" (subs addy 2)))

(defn objectAddy [object]
  (sha/string object))

(defn storeObject [objectInBytes dir database]
  (let [addy (objectAddy objectInBytes)
        path (pathMaker addy (str dir database))]
    (cond
      (not (.exists (io/as-file path)))
      (do (io/make-parents path)
          (io/copy (hash/zip-str objectInBytes) (io/file path))))
    addy))

(defn blobBytes [file]
  (.getBytes (str "100644 " (.getName file) "\000")))

;(defn treeByteMaker [name addy]
;  (.getBytes (str "40000 " name "\000")) (ba/from-hex-string addy)
;  )

(defn hexBlobHeader [file]
  (ba/from-hex-string (hash/sha1-sum (hash/blobNhead file))))

(defn enterBlob [file dir database]
  (hash/store-blob (subs (.getPath file) (count dir)) dir database)
  (ba/concat (blobBytes file) (hexBlobHeader file)))

(defn enterTree [name treeAddy]
  (cond
    (nil? treeAddy) nil
    :else
    (ba/concat (.getBytes (str "40000 " name "\000")) (ba/from-hex-string treeAddy))))

;A3 says to not do writes for nil files
(defn sortContents [filesNdirs]
  (filter #(not (nil? %)) filesNdirs))

;forgot the 0 for reduce printed blank w/o it
(defn levels? [filesNdirs]
  (reduce + 0 (map count filesNdirs)))

(defn findEntries [contents dir database]
  (let [noNil (sortContents contents)
        levels (levels? noNil)
        joinNoNill (apply concat noNil)
        makeTreeBytes (-> (str "tree " levels "\000") .getBytes (concat joinNoNill) byte-array)]
    (cond
      (= (count noNil) 0) nil
      :else
      (storeObject makeTreeBytes dir database))))

;alphasort files
(defn alphaSort [files]
  (sort-by #(.getName %) (rest files)))

(defn filesPerLevel [level AZsorted]
  (filter #(= level (+ (count (re-seq #"\\" (.getPath %))) (count (re-seq #"/" (.getPath %))))) AZsorted))

(defn anythingThere? [%]
  (cond
    (= nil %) (println "The directory was empty, so nothing was saved.")
    :else
    (println %)))
;(declare iterateNbuildTree)

;(defn loopThroughCurrLevel [filesOnLevel level database nextDir]
;  (for [file filesOnLevel]
;    (cond (and (.isDirectory file) (not= (.getName file) database))
;          (enterTree (.getName file) (iterateNbuildTree (inc level) database nextDir file))
;          :else
;          (enterBlob file nextDir database)
;          )
;    )
;  )

(defn iterateNbuildTree [level database nextDir current]
  (let [files (file-seq current)
        ;sort files alphabetically
        AZsorted (alphaSort files)
        filesOnLevel (filesPerLevel level AZsorted)
        contents (for [file filesOnLevel]
                   (if (.isDirectory file)
                     (when (not= (.getName file) database)
                       (enterTree (.getName file) (iterateNbuildTree (inc level) database nextDir file)))
                     ;else
                     (enterBlob file nextDir database)))]
    (findEntries (vec contents) nextDir database)))

; dir = directory -- RS
(defn write-tree [mArgs dir database]
  (cond
    ;helpn cond
    (or (= (first mArgs) "-h") (= (first mArgs) "--help")) (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")

    ;no other args
    (and (not= (count mArgs) 0) (and (not= (first mArgs) "-h") (not= (first mArgs) "--help")))
    (println "Error: write-wtree accepts no arguments")

    ;nil first arg and database check
    (and (nil? (first mArgs)) (not (.exists (io/as-file (str dir database))))) (println "Error: could not find database. (Did you run `idiot init`?)")

    :else
    ;This creates a new tree object for the contents of the current directory
    (->> (io/file dir)
         (iterateNbuildTree (count (re-find (re-pattern "/") dir)) database dir)
         anythingThere?)))