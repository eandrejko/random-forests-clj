(ns random-forests.core)

(defn targets
  "returns collection of targets from examples"
  [examples]
  (map last examples))

(defn target-mode
  "determines the mode of the target in examples"
  [examples]
  (first (last (sort-by val (frequencies (targets examples))))))

(defn target-is-constant?
  "determines if the target is constant within the provided examples, if so returns the constant value, else returns nil"
  [examples]
  (let [s (set (targets examples))]
    (if (= 1 (count s)) (first s))))

(defn gini-impurity
  "computes the Gini impurity coefficient for provided examples and specified feature i (an index into the examples)"
  [targets frequencies]
  (let [k (apply + (vals frequencies))]
    (- 1 (apply + (map #(let [f (/ (get frequencies % 0) k)] (* f f)) targets)))))

(defn avg
  "returns avg value of collection"
  [coll]
  (/ (apply + coll) (count coll)))

(defn split-examples
  "determines the subset of examples with the specified feature having the specified value"
  [examples feature-value]
  (let [i (:feature feature-value)
        val (:value feature-value)]
    {:equal (filter #(= (nth % i) val) examples),
     :unequal (filter #(not (= (nth % i) val)) examples)}))

(defn both-splits-nonempty?
  "determines if both subsets of slip are non-empty"
  [split-examples]
  (and (not (empty? (:equal split-examples))) (not (empty? (:unequal split-examples)))))

(defn measure-split
  "measures the value of splitting the examples using the specified feature value pair using the Gini impurity measure"
  [examples feature-value]
  (let [all-targets (set (targets examples))
        k (:feature feature-value)
        v (:value feature-value)
        split-examples (split-examples examples feature-value)]
    (if (both-splits-nonempty? split-examples)
      (avg (map #(gini-impurity all-targets (frequencies (targets %)))
                (vals split-examples))))))

(defn feature-value
  [feature value]
  {:feature feature, :value value})

(defn feature-values
  "determines set of values for feature"
  [examples feature]
  (set (map #(nth % feature) examples)))

(defn determine-split
  "returns a feature value pair as {:feature feature, :value value} representing the best split of the provided examples from the provided features"
  [examples features]
  (ffirst (sort-by last
                   (filter last
                           (map #(vector % (measure-split examples %))
                                (for [feature features, value (feature-values examples feature)]
                                  (feature-value feature value)))))))

(defn determine-features
  "determines the remaining feature set by removing the specfied feature"
  [features feature-value]
  (disj features (:feature feature-value)))

;; method defined below, needed for call in build-tree-with-split
(defn build-tree [examples features])

(defn build-tree-with-split
  [examples features feature-value]
  (if feature-value
    ;; examples are splittable using feature and value
    (let [ex (split-examples examples feature-value)
          ft (determine-features features feature-value)
          child-eq (build-tree (:equal ex) ft)
          child-neq (build-tree (:unequal ex) ft)]
      (with-meta
        (fn [x]
        (if (= (:value feature-value) (nth x (:feature feature-value)))
          (child-eq x)
          (child-neq x)))
        {:tree (str "if(" (:feature feature-value) "=" (:value feature-value) "){" (:tree (meta child-eq)) "}else{" (:tree (meta child-neq)) "}" )}))
    ;; examples cannot be split all features are identical
    (let [t (target-mode examples)]
      (with-meta (fn [x] t) {:tree t}))))

(defn build-tree
  "builds a decision tree recursively using examples as a collection of vectors with the last element assumed to be the target variable and features a vector of indices to use as features"
  [examples features]
  (if (empty? examples) (throw (Exception. "Examples unexpectedly empty")))
  (if (empty? features)
    ;; no longer have features to split on, so use most common target
    (let [t (target-mode examples)]
      (with-meta (fn [x] t) {:tree t}))
    ;; have feature to split on, find best and recurse
    (let [t (target-is-constant? examples)]
      (if t 
        ;; constant target value amongst examples, so use this target value
        (with-meta (fn [x] t) {:tree t})
        ;; else determine best splitting node and recurse with new examples and features
        (build-tree-with-split examples features (determine-split examples features))))))

(defn classify
  [features tree]
  (tree features))