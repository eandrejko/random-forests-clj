(defproject random-forests-clj "0.1.0"
  :description "An implementation of Random Forests for classification in Clojure"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [clj-tokenizer "0.1.0"]
                 [org.clojure/tools.cli "0.2.1"]]
   :main random-forests.train)
