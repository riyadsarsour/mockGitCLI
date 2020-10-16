(ns cat-file
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [abbreviation :as ab]
            [byte-array :as ba])
  (:import java.io.ByteArrayOutputStream java.util.zip.InflaterInputStream))

;****** START HELPERS Given IN CAT_FILE***************


(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))

(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))
;**********END OF HELPER FOR CAT_FILE*********************

;path maker same  as tree
(defn filePath [dir database addy]
  (str dir "/" database "/objects/"  (subs addy 0 2) "/" (subs addy 2)))

(defn objectType? [dir database addy]
  (->> (filePath dir database addy)
       unzip
       (split-at-byte 32)
       first
       bytes->str))

(defn getBytes [addy]
  (->> addy
       unzip
       (split-at-byte (byte 0x00))
       second))

(defn tree-entry-formatter
  "given a single entry from within a tree object, returns formatted string"
  [header-bytes address-bytes]
  (let [header-string-list (-> header-bytes bytes->str (str/split #" "))
        mode (first header-string-list)
        name (second header-string-list)
        address (ba/to-hex-string address-bytes)
        mode (if (= "40000" mode)
               (str "0" mode)
               mode)
        type (if (= mode "040000")
               "tree"
               "blob")]
    (str mode " " type " " address "\t" name "\n")))

(defn format-tree-output
  [content-bytes]
  (let [split-bytes (->> content-bytes
                         (partition-by (partial = 0x00))
                         (take-nth 2))
        len-1 (-> split-bytes count (- 1))]
    (loop [n 1
           header-bytes (first split-bytes)
           string ""]
      (if (> n len-1)
        string
        (let [all-info (->> (nth split-bytes n) (split-at 20))
              address-bytes (first all-info)
              next-header-bytes (second all-info)]
          (recur (inc n) next-header-bytes (str string (tree-entry-formatter header-bytes address-bytes))))))))


;(defn fileThere? [dir database addy]
;  (cond
;    (.exists (io/as-file (filePath dir database addy))) true
;    :else
;    false)
;  )

;COMMENTD OUT FOR STYLE POINTS RN
;(defn innerObjectAddy [objAddy]
;  (->> (split-at-byte 0 bytes) (second) (split-at-byte 0)
;       (first) (take 20) (ba/to-hex-string))
;  )
;


(defn handleTreeBytes [dir database treeBytes]
  (when
   (> (count treeBytes) 0)
    (let [addy (->> treeBytes (split-at-byte 0) second (split-at-byte 0) first (take 20) ba/to-hex-string)
          typeNname (str/split (->> treeBytes (split-at-byte 0) first bytes->str) #" ")
          otherBytes (->> treeBytes (split-at-byte 0) second (drop 20) byte-array)]
      (str
       (first typeNname) " " (objectType? dir database addy) " "
       addy "\t" (second typeNname) "\n"
       (handleTreeBytes dir database otherBytes)))))

(defn returnTree [dir database addy]
  (let [path (filePath dir database addy)
        treeBytes (->> path unzip (split-at-byte 0) second)]
    (str/replace (->> treeBytes (handleTreeBytes dir database) str/trim) #"40000" "040000")))

(defn handleCommit [dir database addy]
  (let [path (filePath dir database addy)
        bytes (->> path unzip (split-at-byte 0) second)]
    (->> bytes bytes->str)))

;(defn objectBytes [dirDb addy]
;  (let [path (filePath dir database addy)]
;
;    (->> addy
;       path
;       unzip
;       (split-at-byte (byte 0x00))
;       second))
;  )


;(defn type?[dirDB addy]
;  (as-> addy x
;        (filePath dirDB x)
;        (unzip x)
;        (slurp x)
;        (str/split x #"\000")
;        (first x)
;        (apply str x)
;        (str/split x #" ")
;        (first x)))

;(defn readBlob [dirDB addy]
;  (str/split (slurp (unzip (filePath dirDB addy))) #"\000"))


(defn returnBlob [dir database addy]
  (let [path (filePath dir database addy)
        blobInfo (slurp (unzip path))]
    (second (str/split blobInfo #"\000"))))

(defn cat-file [args dir database]
  (let [cmd (first args)
        addy (second args)
        search (ab/search addy dir database)
        match (first search)
        searchAddy (first (second search))
        path #(str dir "/" database "/objects/" (subs % 0 2) "/" (subs % 2))]
    (cond
      ;help messages :P
      (or (= cmd "-h") (= cmd "--help"))
      (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
      ;see if it exists
      (not (.exists (io/file (str dir "/" database))))
      (println "Error: could not find database. (Did you run `idiot init`?)")

      ;see if not enough arguments on -p or not enough args on -t
      (and (or (= cmd "-p") (= cmd "-t")) (= cmd (last args)))
      (println "Error: you must specify an address")

      ;neither p ot t switch or help
      (not (or (= cmd "-p") (= cmd "-t")))
      (println "Error: the -p or -t switch is required")
      ; not enough chars for abbrev
      (< (count addy) 4)
      (println (format "Error: too few characters specified for address '%s'" addy))
      ; no matches found
      (not= 1 match)
      (ab/addyError addy match "Error: that address doesn't exist")
      ;commit-addr hard coded test
      ;(and (= (first args) "-t") (= (second args) "commit-addr")) (println "commit")
      ;address doesnt exist
      ; addy not exist


      ;if everything -p is right check in else
      ;if everything -t is right check in else

      :else (cond
              (= cmd "-p") (if (= (objectType? dir database searchAddy) "tree")
                             (->> searchAddy
                                  path
                                  getBytes
                                  format-tree-output
                                  print)
                             (->> searchAddy
                                  path
                                  getBytes
                                  (map char)
                                  (apply str)
                                  print))
              (= cmd "-t") (println (objectType? dir database searchAddy))))))

;(and (= cmd "-p") (= (objectType? dir database addy) "blob") )
;(print (last (returnBlob dir database addy)))

;(cond
;  ;if -t print object type and \n
;  (= (first args) "-t") (println (type? (str dir database) (second args)))
;
;  ;pswitch n Tree
;  (and (= (first args) "-p") (= (objectType? (filePath (str dir database) (second args))) "tree") )
;  (->> (second args)
;        (objectBytes (str dir database))
;        returnTree
;        print)
;
;  ;blob n p switch
;  (and (= (first args) "-p") (= (objectType? (filePath (str dir database) (second args))) "blob") )
;  (print (last (returnBlob (str dir database) (second args))))
;
;  :else
;  ;(print (returnBlob (str dir database) (second args)))
;  (->> (second args)
;        (objectBytes (str dir database))
;        (map char)
;        (apply str)
;        print)
;  )
; end cat