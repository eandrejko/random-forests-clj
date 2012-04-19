(ns random-forests.core
  (:use [clojure.math.combinatorics]
        [clojure.set])
  (:require [clojure.string :as str]
            [random-forests.stats :as stats]))

(defn targets
  "returns collection of targets from examples"
  [examples]
  (map last examples))

;; target of example
(def target last)

(defn mode
  "determines the mode of a coll"
  [coll]
  (first (last (sort-by val (frequencies coll)))))

(defn target-mode
  "determines the mode of the target in examples"
  [examples]
  (mode (targets examples)))

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
    {:equal (filter feature-value examples),
     :unequal (filter #(not (feature-value %)) examples)})

(defn both-splits-nonempty?
  "determines if both subsets of slip are non-empty"
  [split-examples]
  (and (not (empty? (:equal split-examples))) (not (empty? (:unequal split-examples)))))

(defn measure-split
  "measures the value of splitting the examples using the specified feature value pair using the Gini impurity measure"
  [examples feature-value]
  (let [all-targets (set (targets examples))
        split-examples (split-examples examples feature-value)]
    (if (both-splits-nonempty? split-examples)
      (avg (map #(gini-impurity all-targets (frequencies (targets %)))
                (vals split-examples))))))

(defn feature
  "creates a feature map containing the feature index, type and name"
  ([name i]
     (feature name i :categorical))
  ([name i type]
     (hash-map :name name :index i :type type))
  ([name i type vector-size]
     (hash-map :name name :index i :type type :vector-size vector-size))
  ([name i type vector-size dictionary]
     (hash-map :name name :index i :type type :vector-size vector-size :dict dictionary)))

(defn interaction?
  "determines if a feature is an interaction of two or more features"
  [feature]
  (not (map? feature)))

(defn feature-value
  "creates a filter function for a feature value pair"
  [feature value]
  (let [i (:index feature)]
    (cond
     (interaction? feature)
     (let [truth-conditions (map feature-value feature value)
           text (map #(:text (meta %)) truth-conditions)]
       (with-meta
         (fn [example] (reduce (fn [x y] (and x y)) (map #(% example) truth-conditions)))
         {:feature feature :value value :text (str/join " and " text)}))
     (= :continuous (:type feature))
     (with-meta
       (fn [example] (<= (nth example i) value))
       {:feature feature :value value :text (str (:name feature) "<=" value)})
     (= :text (:type feature))
     (with-meta
       (fn [example] (contains? (nth example i) value))
       {:feature feature :value value :text (str (:name feature) " contains " (if (:dict feature) ((:dict feature) value) value))})
     :else
     (with-meta
       (fn [example] (= (nth example i) value))
       {:feature feature :value value :text (str (:name feature) "==" value)}))))

(defn pairs
  "returns seq of pairs from collection"
  [coll]
  (partition 2 1 coll))

(defn feature-values
  "determines set of values for feature"
  [examples feature]
  (cond
   (interaction? feature)
   (apply cartesian-product (map #(feature-values examples %) feature))
   (= (:type feature) :text)
   (let [values (->> examples
                     (map #(nth % (:index feature)))
                     (reduce union))]
     values)
   (= :continuous (:type feature))
   (let [values (map #(nth % (:index feature)) examples)]
     (set (map #(/ (+ (last %) (first %)) 2) (pairs (sort values)))))
   :else
   (set (map #(nth % (:index feature)) examples))
   ))

(defn determine-split
  "returns a feature value pair as {:feature feature, :value value} representing the best split of the provided examples from the provided features"
  [examples features sample-size]
  (->> (for [feature features
             value   (feature-values examples feature)]
         (feature-value feature value))
       (shuffle)
       (take sample-size)
       (pmap #(vector % (measure-split examples %)))
       (filter last) ;; remove unmeasurable splits
       (sort-by last) ;; best split has minimal measure
       (ffirst)))

(defn determine-features
  "determines the remaining feature set by removing the specfied feature"
  [features feature-value]
  (disj features (:feature (meta feature-value))))

;; method defined below, needed for call in build-tree-with-split
(defn build-tree [examples features])

(defn build-tree-with-split
  [examples features feature-value sample-size]
  (if feature-value
    ;; examples are splittable using feature and value
    (let [ex (split-examples examples feature-value)
          child-eq (build-tree (:equal ex) features sample-size)
          child-neq (build-tree (:unequal ex) features sample-size)]
      (let [mfv (meta feature-value)]
        (with-meta
          (fn [x]
            (if (feature-value x)
              (child-eq x)
              (child-neq x)))
          {:tree (str "if(" (:text mfv) "){" (:tree (meta child-eq)) "}else{" (:tree (meta child-neq)) "}" )})))
    ;; examples cannot be split all features are identical
    (let [t (target-mode examples)]
      (with-meta (fn [x] t) {:tree t}))))

(defn build-tree
  "builds a decision tree recursively using examples as a collection of vectors with the last element assumed to be the target variable and features a vector of indices to use as features"
  [examples features sample-size]
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
        (build-tree-with-split examples features (determine-split examples features sample-size) sample-size)))))

(defn indexed
  "indexes a collection using 0..."
  [coll]
  (map vector (iterate inc 0) coll))

(defn split-dataset-into-training-and-test
  "splits a dataset into a training in test dataset using a 80/20 split"
  [dataset]
  {:training (map last (filter #(< 0 (mod (first %) 5)) (indexed dataset))),
   :test (map last (filter #(= 0 (mod (first %) 5)) (indexed dataset)))})

(defn build-and-evaluate-tree
  "builds a tree from examples using features using m feature per split and bootstrap sample of size k
   returns tree with meta-data :eval which contains a map of {example [prediction] ...} on held out data"
  [examples features m k]
  (let [[examples heldout] (stats/bootstrap-and-heldout examples k)
        tree               (build-tree examples features m)
        evaluation         (->> heldout
                                (map #(vector % [(tree %)]))
                                (into {}))]
    (with-meta tree (assoc (meta tree) :eval evaluation))))

(defn build-random-forest
  "returns a sequence of decision trees using boostrapped training examples
   and using a random sample of m features"
  [examples features m k]
  (repeatedly #(build-and-evaluate-tree examples features m k)))

(defn combine-predictions
  "combines predictions of examples of the form {example [prediction] ...} and returns [target prediction] pairs"
  [predictions]
  (->> predictions
       (reduce (partial merge-with concat))
       (map (fn [[example preds]] (vector (target example) (avg preds))))))

(defn evaluate-forest
  "evaluates collection of trees by averaging predictions on held out data within trees meta-data :eval
   returns collection of [target prediction] pairs"
  [forest]
  (->> forest
       (map (comp :eval meta))
       (combine-predictions)))

(defn votes
  "determines vote of each decision tree in forest"
  [forest features]
  (map #(% features) forest))

(defn classify
  "classifies an example by estimating the probability an example belongs to
   a particular class by taking votes from decision trees in forest"
  [forest features]
  (let [votes (votes forest features)
        k (count forest)
        f (frequencies votes)]
    (reduce (fn [m x] (assoc m x (/ (get f x) k))) {} (keys f))))

(defn classify-scalar
  "returns a number between 0 and 1 for a forest acting as a binary classifer with decision
   trees returning 0 or 1"
  [forest features]
  (avg (votes forest features)))

(defn auc
  "measure the auc of a forest against the provided examples"
  [forest examples]
  (let [scored (reduce
                (fn [h x]
                  (assoc h (first x) (conj (get h (first x) '()) (last x)))) {} (map #(vector (last %) (classify-scalar forest %)) examples))
        one-scores (take 1000 (stats/lazy-sample (get scored 1)))
        zero-scores (take 1000 (stats/lazy-sample (get scored 0)))]
    (float (avg (map #(if (< (first %) (last %)) 1 0)
                     (map vector zero-scores one-scores))))))

;; usage
(comment

  (def data-file "test/data/cancer.csv")

  (def data (split-dataset-into-training-and-test
             ;; the target variable must be read as an integer to measure the auc
             (map
              #(vec (concat (butlast %) (list (Integer/parseInt (last %)))))
              (read-dataset data-file))))

  ;; everything but the last column is an input feature
  (def features (set (map #(feature (str "V" %) %) (range (dec (count (first (:training data))))))))

  (def features-with-interactions (set
                                   (concat
                                   features
                                   (for [a features b features :when (not (= a b))] [a b]))))

  (def forest (doall
               (take 250 (build-random-forest (:training data) features-with-interactions 3))))

  (println "AUC: " (auc forest (:test data)))

  )