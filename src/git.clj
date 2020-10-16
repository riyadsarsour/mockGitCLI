(ns git
  (:require [byte-array :as ba]
            sha))

(defn with-header
  "Return the given data with a header prepended, as a byte array."
  [type data]
  (ba/concat
   (format "%s %d\000" type (count data))
   data))

(defn address
  "Return the address of the given object of the given type. The object is
  assumed not to have a header already."
  [type data-without-header]
  (sha/string (with-header type data-without-header)))