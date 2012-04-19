(ns random-forests.stats)

(defn bootstrap
  "returns a bootstrap sample of the provided collection of size k"
  ([coll]
     (let [N (count coll)]
        (bootstrap coll N)))
  ([coll k]
     (let [N (count coll)]
       (repeatedly k #(nth coll (rand-int N))))))

(defn lazy-sample
  "returns a random sample of the specified size from coll"
  [coll]
  (let [coll (seq coll)
        k (count coll)]
    (repeatedly #(nth coll (rand-int k)))))

(defn sample
  "returns a random sample of the specified size from coll"
  [coll k]
  (take k (lazy-sample coll)))
