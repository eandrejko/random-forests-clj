(ns random-forests.test.core
  (:use [random-forests.core] :reload)
  (:use [clojure.test]))

(deftest target-mode-determines-mode-of-target
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 0] )]
    (is (= 1 (target-mode examples)))))

(deftest target-is-constant?-returns-nil-when-target-not-constant
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 0] )]
    (is (= nil (target-is-constant? examples)))))

(deftest target-is-constant?-returns-target-when-target-is-constant
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 1] )]
    (is (= 1 (target-is-constant? examples)))))

(deftest split-examples-finds-subset-of-examples-matching-feature-value
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 1] )
        feature (feature "gender" 0)]
    (is (= (list ["F" "<30" 1]) (:equal (split-examples examples
                                                        (feature-value feature "F")))))))

(deftest determine-features-removes-feature-from-feature-val
  (let [features (set (map #(feature % %) #{0 1}))]
    (is (= #{(feature 1 1)} (determine-features features (feature-value (feature 0 0) "F"))))))

(deftest build-tree-with-empty-features-maps-to-mode-of-target
  (let [features #{}, examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 0] )]
    (is (= 1 ((build-tree examples features) ["M" "<25" 1])))))

(deftest build-tree-with-constant-target
  (let [features (set (map #(feature % %) #{0}))
        examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 1] )]
    (is (= 1 ((build-tree examples features) ["M" "<25" 0])))))

(deftest gini-impurity-with-constant-target-value-is-zero
  (let [targets #{0 1}, frequencies {1 3}]
    (is (= 0 (gini-impurity targets frequencies)))))

(deftest gini-impurity-with-even-frequencies-is-maximal
  (let [targets #{0 1}, frequencies {1 3 0 3}]
    (is (= (- 1 (+ 1/4 1/4)) (gini-impurity targets frequencies)))))

(deftest measure-split-measure-average-gini-impurity-of-examples-split-by-feature-with-optimal-split
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] )
        fv (feature-value (feature "age" 1) "<30")]
    (is (= 0 (measure-split examples fv)))))

(deftest measure-split-measure-average-gini-impurity-of-examples-split-by-feature-with-poor-split
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] ["F" "<30" 0] )
        fv (feature-value (feature "gender" 0) "M")]
    (is (= 1/2 (measure-split examples fv)))))

(deftest measure-split-is-nil-for-constant-feature-value
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["M" "<30" 0] ["M" "<30" 0] )
        fv (feature-value (feature "gender" 0) "M")]
    (is (nil? (measure-split examples fv)))))

(deftest feature-values-determines-set-of-feature-values
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] ["F" "<30" 0] )
        s #{"M" "F"}]
    (is (= s (feature-values examples (feature "gender" 0))))))

(deftest feature-values-splits-continuous-values-at-midpoints
  (let [examples (list ["M" 28 0] ["M" 30 1] ["F" 30 1] ["F" 31 0] )
        s #{29 30 61/2}]
    (is (= s (feature-values examples (feature "age" 1 :continuous))))))

(deftest determine-split-chooses-split-with-minimal-avg-gini-impurity
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<25" 0] ["F" "<35" 1] )
        fv (feature-value (feature 1 1) "<25")
        features (set (map #(feature % %) #{0 1}))
        split (determine-split examples features)]
    (is (= (:feature (meta fv)) (:feature (meta split))))
    (is (= (:value (meta fv)) (:value (meta split))))))

(deftest feature-value-uses-order-operator-with-continuous-values
  (let [fv (feature-value (feature "age" 1 :continuous) 30.5)]
    (is (= false (fv ["M" 31])))
    (is (= true (fv ["M" 30])))
    (is (= true (fv ["M" 30.5])))))

(deftest determine-split-chooses-best-split-for-continuous-feature-and-splits-at-midpoint
  (let [examples (list ["M" 24 0] ["M" 30 1] ["F" 25 0] ["F" 35 1] )
        fv (feature-value (feature "age" 1 :continuous) 27.5)
        features #{(feature "gender" 0) (feature "age" 1 :continuous)}
        split (determine-split examples features)]
    (is (= (:feature (meta fv)) (:feature (meta split))))
    (is (= (:value (meta fv)) (:value (meta split))))))

(deftest both-splits-nonempty?-returns-false-when-one-split-is-empty
  (let [examples (list ["M" "<25" 0] ["M" "<25" 1])]
    (is (= false (both-splits-nonempty? (feature-value (feature "age" 0) "<25"))))))

(deftest determine-split-returns-nil-when-no-split-possible
  (let [examples (list ["M" "<25" 0] ["M" "<25" 1])
        features (set (map #(feature % %) #{0 1}))]
    (is (= nil (determine-split examples features)))))

(deftest build-tree-builds-decision-tree
  (let [examples (list ["M" "<25" 0] ["M" "<25" 0] ["F" "<30" 1] ["F" "<30" 1] )
        features (set (map #(feature % %) #{0 1}))]
    (is (= 0 ((build-tree examples features) ["M" "<25"])))))
