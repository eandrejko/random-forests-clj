(ns random-forests.test.stats
  (:use random-forests.stats :reload)
  (:use [clojure.test]
        [clojure.set]))

(deftest bootstrap-takes-bootstrap-sample
  (let [sample (bootstrap (range 100))]
    (is (every? (set (range 100)) sample))))

(deftest bootstrap-of-size-k-takes-bootstrap-sample-of-size-k
  (let [sample (bootstrap (range 100) 15)]
    (is (every? (set (range 100)) sample))
    (is (= 15 (count sample)))))
