(ns random-forests.train
  (:use [clojure.tools.cli :only (cli)]
        [random-forests.encoding :only (text-tokens)])
  (:require [clojure-csv.core :as csv]
            [random-forests.core :as rf])
  (:gen-class :main true))

(defn named-examples
  "converts a header row and collection if rows to name-value pairs"
  [header input]
  (->> input
       (map #(map vector header %))))

(defn encoder-fn
  "returns encoder function for feature of type kind"
  [kind]
  (case kind
    "text"       (fn [x] (set (text-tokens x)))
    "continuous" (fn [x] (Double/parseDouble x))
    identity))

(defn csv-from-path
  "reads csv file from path"
  [path]
  (-> path
      slurp
      csv/parse-csv))

(defn encoding-fns
  "returns map of feature name to encoding-fn"
  [feature-desc]
  (->> feature-desc
       (map #(clojure.string/split % #"="))
       (map (fn [[name kind]] [name (encoder-fn kind)]))
       (into {})))

(defn features
  "returns collection of features from feature description"
  [header feature-desc]
  (let [header (->> header
                    (map vector (iterate inc 0))
                    (map reverse)
                    (map vec)
                    (into {}))]
    (->> feature-desc
       (map #(clojure.string/split % #"="))
       (map (fn [[name kind]] (rf/feature name (header name) (keyword kind)))))))

(defn -main
  [& args]
  (let [[options args banner] (cli args
                                   ["-h" "--help" "Show help" :default false :flag true]
                                   ["-f" "--features" "Features with types to use for prediction, comma separated with names matching header: name=continuous,foo=text" :parse-fn #(clojure.string/split % #",") :default []]
                                   ["-t" "--target" "Prediction target name"])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (let [input            (csv-from-path (first args))
          [header & input] input
          encoding         (encoding-fns (conj (:features options) (:target options)))
          target-name      (-> (clojure.string/split (:target options) #"=") first)
          target-index     (->> header
                                (keep-indexed (fn [i x] (if (= x target-name) i)))
                                (first))
          examples         (->> (named-examples header input)
                                (map #(map (fn [[name val]] ((get encoding name identity) val)) %))
                                (map vec)
                                (map (fn [z] (conj z (nth z target-index))))) ;; target is at end
          features         (set (features header (:features options)))]
      (let [tree        (rf/build-tree examples features)
            predictions (map tree examples)
            errors      (map (fn [a b] (Math/abs (- a b))) predictions (map last examples))]
        (println (/ (reduce + errors) (count errors))))
      (shutdown-agents))))