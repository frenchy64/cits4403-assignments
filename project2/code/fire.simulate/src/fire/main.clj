(ns fire.main
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.core.typed :refer [ann check-ns print-env]]
            [clojure.java.io :refer [reader]]
            [fire.simulate.percolation :as perc])
  (:gen-class))

(ann run-percolation [Number -> nil])
(declare run-percolation)
;(defn run-percolation [q]
;  (letfn> [parse-input :- [String -> nil]
;           (parse-input [in-str]
;             (let [kw (keyword in-str)]
;               (cond
;                 (#{:next} kw)
;  (let [g0 (perc/initial-grid :q q)]
;    (with-open [^java.io.Reader rdr (reader *in*)]
;      (loop [c (.read rdr)])))))

(ann -main [String * -> nil])
(defn -main [& args]
  (let [[options args banner]
        (cli args
             ["--percolation" "Run the percolation example" :default false, :flag true]
             ["--rows" "Number of rows in simulation." :default 100, :flag false, :parse-fn #(Long/parseLong %)]
             ["--cols" "Number of columns in simulation." :default 100, :flag false, :parse-fn #(Long/parseLong %)]
             ["-q" "Probability of generating a tree in starting grid." :default 0.5, :flag false, :parse-fn #(Double/parseDouble %)]
             ["-h" "--help" "Display usage." :default false, :flag true])]
    (print-env "cli")
    (when (:help options) 
      (println banner)
      (System/exit 0))
    (cond
      (:percolation options)
      (run-percolation (:q options_)))))
