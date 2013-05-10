(ns fire.main
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.core.typed :refer [ann check-ns print-env letfn> typed-deps
                                        ann-protocol loop>]]
            [clojure.core.typed.hole :refer [silent-hole]]
            [clojure.tools.analyzer.hygienic :as hy]
            [clojure.java.io :refer [reader]]
            [clojure.string :as str]
            [fire.gnuplot :as plot]
            [fire.simulate
             [percolation :as perc]])
  (:import (clojure.lang Seqable))
  (:gen-class))

(typed-deps fire.types
            fire.gnuplot
            fire.simulate
            fire.simulate.percolation
            clojure.core.typed.hole)

(ann run-percolation [Number -> nil])
(defn run-percolation [p]
  (let [proc (plot/start)]
    (letfn> [clean-up :- [-> nil]
             (clean-up []
                       (println "Destroying gnuplot ...")
                       (plot/stop proc)
                       (println "Process destroyed"))

             parse-input :- [(Seqable String) -> nil]
             (parse-input [strs]
                          (silent-hole)

                          #_(case kw
                              :exit 
                              :next ))]
      (let [g0 (perc/initial-grid :p p)]
        (with-open [rdr (reader *in*)]
          (loop> [c :- (Seqable String), (line-seq rdr)]
            (parse-input c)
            (recur (line-seq rdr))))))))

(ann entry-point [-> nil])
(defn entry-point []
  (with-open [rdr (reader *in*)]
    (loop> [c :- (Seqable String), (line-seq rdr)]
      (silent-hole)
      (recur (line-seq rdr)))))

(ann -main [String * -> nil])
(defn -main 
  "Main entry point."
  [& args]
  (entry-point))
;  (let [[options args banner]
;        (cli args
;             ["--percolation" "Run the percolation example" :default false, :flag true]
;             ["--rows" "Number of rows in simulation." :default 100, :flag false, :parse-fn #(Long/parseLong %)]
;             ["--cols" "Number of columns in simulation." :default 100, :flag false, :parse-fn #(Long/parseLong %)]
;             ["-q" "Probability of generating a tree in starting grid." :default 0.5, :flag false, :parse-fn #(Double/parseDouble %)]
;             ["-h" "--help" "Display usage." :default false, :flag true])]
;    (when (:help options) 
;      (println banner)
;      (System/exit 0))
;    (cond
;      (:percolation options)
;      (run-percolation (:q options)))))
