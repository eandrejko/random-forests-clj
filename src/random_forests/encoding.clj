(ns random-forests.encoding)

(defn encode-text-into-vector
  "encodes text into a binary vector of the specified size"
  [coll size]
  (let [hash-values (set (map #(mod (.hashCode %) size) coll))]
    (vec (map #(if (contains? hash-values %) 1 0) (range 0 (dec size))))))
