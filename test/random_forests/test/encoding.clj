(ns random-forests.test.encoding
  (:use [random-forests.encoding] :reload)
  (:use [random-forests.core]
        [clojure.test]))

(deftest feature-value-selects-text-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["M" (encode-text-into-vector #{"the" "shoe"} 5) 1])
        fv (feature-value (feature "description" 1 :text) (mod (.hashCode "hat") 5))]
    (is (= true (fv (first examples))))
    (is (= false (fv (last examples))))))

(deftest feature-values-determines-set-of-values-for-text-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["M" (encode-text-into-vector #{"the" "shoe"} 5) 1])
        feature (feature "description" 1 :text 5)]
    (is (= (range 0 5) (feature-values examples feature)))))

(deftest feature-value-selects-text-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["M" (encode-text-into-vector #{"the" "shoe"} 5) 1])
        fv (feature-value (feature "description" 1 :text) (mod (.hashCode "hat") 5))]
    (is (= true (fv (first examples))))
    (is (= false (fv (last examples))))))

(deftest feature-values-determines-set-of-values-for-text-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["M" (encode-text-into-vector #{"the" "shoe"} 5) 1])
        feature (feature "description" 1 :text 5)]
    (is (= (range 0 5) (feature-values examples feature)))))

(deftest feature-value-selects-interaction-of-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["F" (encode-text-into-vector #{"the" "hat"} 5) 1])
        fv (feature-value [(feature "description" 1 :text) (feature "gender" 0)] [(mod (.hashCode "hat") 5) "M"])]
    (is (= true (fv (first examples))))
    (is (= false (fv (last examples))))))

(deftest feature-values-determines-set-of-values-for-interaction-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["F" (encode-text-into-vector #{"the" "hat"} 5) 1])
        feature [(feature "description" 1 :text 5) (feature "gender" 0)]]
    (is (= (for [x (range 0 5) y ["F" "M"]] (list x y)) (feature-values examples feature)))))

(deftest feature-value-selects-interaction-of-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["F" (encode-text-into-vector #{"the" "hat"} 5) 1])
        fv (feature-value [(feature "description" 1 :text) (feature "gender" 0)] [(mod (.hashCode "hat") 5) "M"])]
    (is (= true (fv (first examples))))
    (is (= false (fv (last examples))))))

(deftest feature-values-determines-set-of-values-for-interaction-features
  (let [examples  (list ["M" (encode-text-into-vector #{"the" "hat"} 5) 0] ["F" (encode-text-into-vector #{"the" "hat"} 5) 1])
        feature [(feature "description" 1 :text 5) (feature "gender" 0)]]
    (is (= (for [x (range 0 5) y ["F" "M"]] (list x y)) (feature-values examples feature)))))
