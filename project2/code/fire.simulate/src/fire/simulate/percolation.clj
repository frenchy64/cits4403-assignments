(ns fire.simulate.percolation
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst into-array>
                                        override-method Atom1 letfn>]]
            [fire.gnuplot :as plot :refer [GnuplotP]]
            [fire.simulate :as sim]))

(typed-deps fire.simulate fire.gnuplot)

(ann current-proc GnuplotP)
(def current-proc (plot/start))

(sim/setup-gnuplot! current-proc)

(ann my-grid (Atom1 sim/Grid))

(ann frame-number (Atom1 Number))
(def frame-number (atom 0))

(ann my-opts '{:p Number :f Number})
(def my-opts {:p 0.1 :f 0.1})

(ann initial-grid [& :optional {:rows Long, :cols Long} :mandatory {:q Number} -> sim/Grid])
(defn initial-grid
  "Return the initial grid state, a vector of vectors, with each
  position initialised to :empty. If not provided, number of rows and column default
  to 100."
  [& {:keys [rows cols q] :or {rows 100 cols 100}}]
  (letfn> [state-at :- [-> sim/State]
           (state-at []
             (if (sim/occurs? q) 
               :tree 
               :empty))]
    (vec
      (repeatedly
        rows
        (fn [] (vec (repeatedly cols state-at)))))))

(def my-grid (atom (initial-grid :q 0.1)))

(ann my-next [-> nil])
(defn my-next []
  (swap! my-grid sim/next-grid my-opts)
  ; FIXME why do I need to wrap call to inc?
  (swap! frame-number (fn> [x :- Number] (inc x)))
  (sim/update-simulation! current-proc @my-grid {:time-code @frame-number})
  nil)

#_(dotimes [_ 100] (my-next))
