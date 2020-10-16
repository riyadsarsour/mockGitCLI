(ns byte-array
  (:refer-clojure :exclude [cast concat]))

(defn cast
  "Return the given value as a byte array. Works for strings or byte arrays."
  [str-or-byte-array]
  (if (string? str-or-byte-array)
    (.getBytes str-or-byte-array)
    str-or-byte-array))

(defn concat
  "Concatenate multiple items into a byte array."
  [& byte-arrays]
  (byte-array
   (mapcat cast byte-arrays)))

;(defn- byte->hex-digits [byte]
;  (format "%02x"
;         (bit-and 0xff byte)))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn hex-digits->byte
  [[dig1 dig2]]
  ;; This is tricky because something like "ab" is "out of range" for a
  ;; Byte, because Bytes are signed and can only be between -128 and 127
  ;; (inclusive). So we have to temporarily use an int to give us the room
  ;; we need, then adjust the value if needed to get it in the range for a
  ;; byte, and finally cast to a byte.
  (let [i (Integer/parseInt (str dig1 dig2) 16)
        byte-ready-int (if (< Byte/MAX_VALUE i)
                         (byte (- i 256))
                         i)]
    (byte byte-ready-int)))

(defn from-hex-string
  [hex-str]
  (byte-array (map hex-digits->byte (partition 2 hex-str))))