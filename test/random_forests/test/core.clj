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
  (let [examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 1] )]
    (is (= (list ["F" "<30" 1]) (:equal (split-examples examples {:feature 0, :value "F"}))))))

(deftest determine-features-removes-feature-from-feature-val
  (let [features #{0 1}]
    (is (= #{1} (determine-features features {:feature 0, :value "F"})))))

(deftest build-tree-with-empty-features-maps-to-mode-of-target
  (let [features #{}, examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 0] )]
    (is (= 1 ((build-tree examples features) ["M" "<25" 1])))))

(deftest build-tree-with-constant-target
  (let [features #{0}, examples (list ["M" "<25" 1] ["M" "<30" 1] ["F" "<30" 1] )]
    (is (= 1 ((build-tree examples features) ["M" "<25" 0])))))

(deftest gini-impurity-with-constant-target-value-is-zero
  (let [targets #{0 1}, frequencies {1 3}]
    (is (= 0 (gini-impurity targets frequencies)))))

(deftest gini-impurity-with-even-frequencies-is-maximal
  (let [targets #{0 1}, frequencies {1 3 0 3}]
    (is (= (- 1 (+ 1/4 1/4)) (gini-impurity targets frequencies)))))

(deftest measure-split-measure-average-gini-impurity-of-examples-split-by-feature-with-optimal-split
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] )
        fv {:feature 1, :value "<30" }]
    (is (= 0 (measure-split examples fv)))))

(deftest measure-split-measure-average-gini-impurity-of-examples-split-by-feature-with-poor-split
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] ["F" "<30" 0] )
        fv {:feature 0, :value "M" }]
    (is (= 1/2 (measure-split examples fv)))))

(deftest feature-values-determines-set-of-feature-values
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<30" 1] ["F" "<30" 0] )
        s #{"M" "F"}]
    (is (= s (feature-values examples 0)))))

(deftest determine-split-chooses-split-with-minimal-avg-gini-impurity
  (let [examples (list ["M" "<25" 0] ["M" "<30" 1] ["F" "<25" 0] ["F" "<35" 1] )
        fv {:feature 1, :value "<25" }]
    (is (= fv (determine-split examples #{0 1})))))

(deftest both-splits-nonempty?-returns-false-when-one-split-is-empty
  (let [examples (list ["M" "<25" 0] ["M" "<25" 1])]
    (is (= false (both-splits-nonempty? (feature-value 0 "<25"))))))

(deftest determine-split-returns-nil-when-no-split-possible
  (let [examples (list ["M" "<25" 0] ["M" "<25" 1])]
    (is (= nil (determine-split examples #{0 1})))))

(deftest build-tree-builds-decision-tree
  (let [examples (list ["M" "<25" 0] ["M" "<25" 0] ["F" "<30" 1] ["F" "<30" 1] )]
    (is (= 0 ((build-tree examples #{0 1}) ["M" "<25"])))))