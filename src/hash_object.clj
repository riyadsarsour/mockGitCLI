(ns hash-object (:require [clojure.java.io :as io]
                          [sha :as sha])
    (:import java.security.MessageDigest)
    (:import java.util.zip.DeflaterOutputStream (java.io ByteArrayInputStream ByteArrayOutputStream)))

;*************** HELPERS PROVIDED FOR HASH_OBJECT***********
(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))
;*************** END HELPERS OF HASH************************

;********* A3 helpers for blob calculations ***********
;Part of refactoring done from A2
;(defn contents [file]
;  (slurp file))

(defn blobNhead [file]
  (let [data (slurp file)]
    (str "blob " (count data) "\000" data)))

(defn blobaddy [blobHead]
  (sha/sha-bytes (.getBytes blobHead)))

(defn fileAddy [dir file]
  (sha1-sum (blobNhead (str dir file))))

(defn makeBlobPath [addy dirDB]
  (str dirDB "/objects/" (subs addy 0 2) "/" (subs addy 2)))

(defn store-blob [file dir database]
  (let [blobHead (blobNhead (str dir file)) addy (sha1-sum blobHead)
        path (makeBlobPath addy (str dir database))]
    (if (not (.exists (io/as-file path)))
      (do (io/make-parents path)
          (io/copy (zip-str blobHead) (io/file path)))
      (println "Path already exists"))))


;********* END A3 helpers for blob calculations END***********
;refactored Hash-object A3


(defn hash-object [args dir database]
  (let [toDo (first args)
        restArgs (rest args)
        second (first restArgs)]
    (cond
      ; check print help
      (or (= toDo "-h") (= "--help" toDo))
      (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")

      ;check no file specified
      (and (= "-w" toDo) (nil? second)) (println "Error: you must specify a file.")
      (nil? toDo) (println "Error: you must specify a file.")

      ;not in database
      (not (.isDirectory (io/file dir database))) (println "Error: could not find database. (Did you run `idiot init`?)")

      ;if tdo is -w n check if path does not exist
      (and (= "-w" toDo) (.exists (io/as-file (str dir second))))
      (do
        (println (fileAddy dir second))
        ; path make and store it
        (store-blob second dir database))

      ;-w and not readable
      (and (= "-w" toDo) (not (.exists (io/as-file (str dir second))))) (println "Error: that file isn't readable")

      ;if addy exists
      :else
      (if (.exists (io/as-file (str dir toDo))) (println (fileAddy dir toDo))
          (println "Error: that file isn't readable")))))

;hash help
;(defn hashHelp[write, name] (
;                             (if (= "-w" write)
;                               (do (.mkdir (File.
;                                     (str ".git/objects/" (subs (sha1-sum (str "blob " (count (slurp name)) "\000" (slurp name))) 0 2))))
;                                        (io/copy (zip-str (str "blob " (count (slurp name)) "\000" (slurp name)))
;                                          (io/file (str ".git/objects/" (subs (sha1-sum (str "blob " (count (slurp name)) "\000" (slurp name))) 0 2)
;                                                        "/" (subs (sha1-sum (str "blob " (count (slurp name)) "\000" (slurp name))) 2))))
;                                 (println (sha1-sum (str "blob " (count (slurp name)) "\000" (slurp name)))))
;
;                               ;else return blobheader
;                               (println (sha1-sum (str "blob " (count (slurp name)) "\000" (slurp name))))
;                               )
;                             )
;  )


;hash-object A2
;(defn hash-object [args dir database]
;  (let [toDo (first args)
;       rest (rest args)
;        file (first rest)]
;  (cond
;    ;help messages
;    (or (= toDo "-h") (= toDo "--help") )
;    (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
;    ;see if it exists
;    (not (.exists (io/as-file (str dir database)))) (println "Error: could not find database. (Did you run `idiot init`?)")
;    ;now find no -w
;    (and (= 1 (count args)) (not= "-w" toDo) ) (if (.isFile (io/file file)) (hashHelp "" (first args))
;                                                                            (println "Error: that file isn't readable"))
;    ;redo
;    (and (not= "-w" toDo) (not (.exists (io/as-file (str dir file))))) (println "Error: that file isn't readable")
;
;    ; with w
;    (and (= 2 (count args)) (= "-w" (first args)) ) (if (.isFile (io/file (second args))) (hashHelp "-w" (second args))
;                                                                                          (println "Error: that file isn't readable"))
;
;    :else (println "Error: you must specify a file.")
;    )
;  )   ;end let
;  );end hash