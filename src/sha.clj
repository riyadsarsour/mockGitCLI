(ns sha
  (:refer-clojure :exclude [bytes])
  (:require [clojure.java.io :as io]
            [byte-array :as ba])
  (:import java.util.zip.DeflaterOutputStream
           (java.io ByteArrayInputStream
                    ByteArrayOutputStream)
           java.security.MessageDigest))

(defn bytes [data]
  (.digest (MessageDigest/getInstance "sha1") data))

(defn zipStr [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn string [data]
  (-> data bytes ba/to-hex-string))

(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))