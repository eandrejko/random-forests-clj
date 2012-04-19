(ns random-forests.encoding
  (:require [clj-tokenizer.core :as tok]))

(defn text-tokens
  [str]
  (-> str
      (tok/token-stream-without-stopwords)
      (tok/stemmed)
      (tok/token-seq)))

(defn hash-str
  "determines hash value of str in [0,k)"
  [str k]
  (mod (.hashCode str) k))

(defn encode-text-into-vector
  "encodes text into a binary vector of the specified size"
  [coll size]
  (let [hash-values (set (map #(hash-str % size) coll))]
    (vec (map #(if (contains? hash-values %) 1 0) (range 0 size)))))

(defn text-as-vector
  "encodes stemmed tokenized text string as vector of length k"
  [str k]
  (-> str
      (text-tokens)
      (encode-text-into-vector k)))
