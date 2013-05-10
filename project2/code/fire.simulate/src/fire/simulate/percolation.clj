(ns fire.simulate.percolation
  "This namespaces defines the initial grid function for
  the percolation problem"
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst into-array>
                                        override-method Atom1 letfn> ann-form]]
            [clojure.math.numeric-tower :refer [floor abs]]
            [clojure.tools.analyzer.hygienic :refer [macroexpand]]
            [clojure.reflect :as reflect]
            [fire.gnuplot :as plot]
            [fire.simulate :as sim]))

(typed-deps fire.simulate 
            fire.gnuplot)

;--------------------------------------------------
; Types
;--------------------------------------------------

(def-alias PercolationP 
  "The state of a percolation run.

  - :gnuplot  the actual gnuplot process
  - :grid     a grid used to update gnuplot
  - :p        the probability of tree growth at every time interval
  - :frame    the frame number corresponding to the grid, displayed in the gnuplot graph"
  '{:gnuplot plot/GnuplotP,
    :grid sim/Grid,
    :p Number
    :f Number
    :frame AnyInteger})

;--------------------------------------------------
; Grid operations
;--------------------------------------------------

(ann initial-grid [& {:rows Long, :cols Long :q Number} -> sim/Grid])
(defn initial-grid
  "Return the initial grid. States are :tree with probability q, and :empty
  with probability 1 - q.
  Ignites a small patch of trees in the centre of the grid.
  If not provided, number of rows and column default to 100.
  q defaults to 0.1."
  [& {:keys [rows cols q] :or {rows 100, cols 100, q 0.1}}]
  (letfn> [burning? :- [sim/Point -> Boolean]
           (burning? 
             ;Ignite a small patch of trees in the centre.
             [[row col]]
             (let [mid-row (floor (/ rows 2))
                   mid-col (floor (/ cols 2))]
               (boolean
                 (when (and (<= 0 mid-row)
                            (<= 0 mid-col))
                   (and (<= (abs (- row mid-row)) 2)
                        (<= (abs (- col mid-col)) 2))))))

           gen-state :- [sim/Point -> sim/State]
           (gen-state
             ;State is a :tree with probability q, :empty
             ;with probability 1-q, or :burning if part of the
             ;small patch of trees in the centre of the grid.
             [pnt]
             (cond 
               (burning? pnt) :burning
               (sim/occurs? q) :tree 
               :else :empty))]
    (sim/grid-from-fn gen-state :rows rows, :cols cols)))

;--------------------------------------------------
; gnuplot ops
;--------------------------------------------------

(ann start! [& {:q Number, :grid sim/Grid, :p Number, :f Number} -> PercolationP])
(defn start! 
  "Start a gnuplot process and initialize it to the starting
  state. An initial grid can be optionally provided via the :grid keyword argument.
  :q is the :q passed to initial-grid.
  :p is the probability of a tree growing in an empty square.
  :f is the probability of lightning.
  Returns a map for percolation ops."
  [& {:keys [q p f grid] :or {q 0.1, p 0, f 0}}]
  (let [grid0 (or grid (initial-grid :q q))
        proc (plot/start)
        frame0 0]
    (sim/setup-gnuplot! proc)
    (sim/update-simulation! proc grid0 :time-code frame0)
    {:gnuplot proc,
     :grid grid0,
     :p p,
     :f f,
     :frame frame0}))

(ann next! [PercolationP -> PercolationP])
(defn next! 
  "Calculate the next state for the percolation plot, and update
  the gnuplot process. Returns a new map for percolation ops."
  [{grid0 :grid, frame0 :frame, :keys [p f gnuplot] :as perc}]
  (let [grid1 (sim/next-grid grid0 {:p p, :f f}) ;no possibility of lightning
        frame1 (inc frame0)]
    (sim/update-simulation! gnuplot grid1 :time-code frame1)
    (-> perc
        (assoc :grid grid1)
        (assoc :frame frame1))))

(comment
(ann my-next [-> nil])
(defn my-next []
  (swap! my-grid sim/next-grid my-opts)
  ; FIXME why do I need to wrap call to inc?
  (swap! frame-number (fn> [x :- Number] (inc x)))
  (sim/update-simulation! current-proc @my-grid {:time-code @frame-number})
  nil)
  )

(comment
(def proc (atom (start! :q 0.1 :p 0 :f 0)))

  (dotimes [_ 100]
    (swap! proc next!))
  )
