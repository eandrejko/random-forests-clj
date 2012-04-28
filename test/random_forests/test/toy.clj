(ns random-forests.test.toy
  (:use [random-forests.core] :reload)
  (:use [clojure.test]))

;; test of random forest models on test problems
;; some problems are toy problems

(defn approx?
  [x y]
  (> 0.1 (Math/abs (- x y))))

(deftest simple-target-leak
  (let [examples [[1 1]]
        features #{(feature "only" 0)}
        forest   (->> (build-random-forest examples features 1 1)
                      (take 10))]
    (is (= 1 (classify-scalar forest (first examples))))))

(deftest simple-classification
  (let [examples (concat
                  (repeatedly 100 (fn [] ["M" 1]))
                  (repeatedly 100 (fn [] ["Z" 2])))
        features #{(feature "only" 0)}
        forest   (->> (build-random-forest examples features 2 100)
                      (take 100))]
    (is (= 1 (classify-scalar forest (first examples))))
    (is (= 2 (classify-scalar forest (last examples))))
    (is (= 0 (->> (evaluate-forest forest avg) ;; held out data
                  (map (fn [[a b]] (- a b)))
                  (reduce +))))))

(deftest simple-regression
  (let [examples   (->> (range 100)
                        (map (fn [z] [z (* 2 z)])))
        features   #{(feature "only" 0 :continuous)}
        forest     (->> (build-random-forest examples features 100 100)
                        (take 50))
        evaluation (evaluate-forest forest avg)] ;; held out data
    ;; ratio between input and output should be approximately two
    (is (approx? 2 (->> (range 1 100)
                        (map (fn [z] [z (classify-scalar forest [z z])]))
                        (map (fn [[a b]] (/ b a)))
                        (avg)
                        (float))))
    (is (> 2 (->> evaluation
                  (map (fn [[a b]] (* (- a b) (- a b))))
                  (avg)
                  (float))))))