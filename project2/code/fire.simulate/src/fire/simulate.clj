; This namespace is the main driver for the fire simulation.

(ns fire.simulate
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst]]
            [clojure.core.typed.hole :as h]
            [fire.gnuplot :as plot :refer [GnuplotP]]
            [clojure.string :as str])
  (:import (clojure.lang IPersistentVector IPersistentSet Seqable)))

; Type check these namespaces before the current one.
(typed-deps fire.gnuplot clojure.core.typed.hole)

; # Define some type aliases
; - Grid is a vector of vectors with leafs either the keyword :burning, :tree or :empty.
(def-alias State (U ':burning ':tree ':empty))
(def-alias Grid (IPersistentVector (IPersistentVector State)))
(def-alias Point '[AnyInteger AnyInteger])

(def-alias GridOpt '{:p Number
                     :f Number})

(ann state->number [State -> Long])
(defn state->number
  "Convert the keyword representation of a state to
  a number usable by Gnuplot for plotting color gradient."
  [k]
  (case k
    :empty 0
    :tree 1
    :burning 2))

(ann occurs? [Number -> Boolean])
(defn occurs?
  "Return true with probability p, otherwise false."
  [p]
  (< (rand) p))

(ann initial-grid [& {:rows Long, :cols Long} -> Grid])
(defn initial-grid
  "Return the initial grid state, a vector of vectors, with each
  position initialised to :empty. If not provided, number of rows and column default
  to 100."
  [& {:keys [rows cols] :or {rows 100 cols 100}}]
  (let [c (vec (repeat cols :empty))]
    (vec (repeat rows c))))

(ann state-at [Grid Point -> State])
(defn state-at [gr [row col]]
  (-> gr
      (nth row)
      (nth col)))

(ann neighbour-positions (IPersistentSet '[Long Long]))
(def neighbour-positions
  "The set of relative (row, col) coordinates to calculate
  the nearest neighbours for a point."
  #{[-1 -1] ;lower left
    [0 -1]  ;left      
    [1 -1]  ;upper left
    [1 0]   ;upper
    [1 1]   ;upper right
    [0 1]   ;right
    [-1 1]  ;lower right
    [-1 0]  ;lower
    })

(ann nearest-neighbours [Grid Point -> (IPersistentSet State)])
(defn nearest-neighbours 
  "Return the set of states of the nearest neighbours
  of the point (row,col) in grid"
  [grid [row col]]
  (let [upper-row (dec (count grid))
        upper-col (dec (count (first grid)))
        ; check row and col are between grid bounds
        _ (assert (<= 0 row upper-row) "Row out of bounds")
        _ (assert (<= 0 col upper-col) "Column out of bounds")
        neighbour-states (set
                           (for> :- State 
                                 [[row-diff col-diff] :- '[Long Long] neighbour-positions
                                  ;filter out points outside the boundary
                                  :when (and (<= 0 (+ row row-diff) upper-row)
                                             (<= 0 (+ col col-diff) upper-col))]
                             (state-at grid [(+ row row-diff) (+ col col-diff)])))]
    neighbour-states))

(ann ^:nocheck clojure.core/rand (Fn [-> Number]
                                     [Number -> Number]))

(ann next-state [Grid State Point GridOpt -> State])
(defn next-state 
  "Return the state at the next time interval in grid for
  the given point, with current state s."
  [grid s [row col :as point] {:keys [f p] :as opt}]
  (let [neighbours (nearest-neighbours grid [row col])]
    (case s
      ;1. A burning tree becomes an empty site
      :burning :empty

      ;2. A viable tree becomes a burning tree if at least one of its nearest neighbours
      ;is burning
      ;4. A tree without a burning nearest neighbour becomes a burning tree during
      ;one time step with probability f (e.g. lightning).
      :tree 
      (if (:burning neighbours)
        :burning
        (if (occurs? f) 
          :burning
          :tree))

      ;3. At an empty site, a tree grows with probability p
      :empty (if (occurs? p)
               :tree
               :empty))))

(ann next-grid [Grid GridOpt -> Grid])
(defn next-grid 
  "Simultaneously update a Grid to the next time increment
  according to the 4 update rules. See next-state for the state 
  increment."
  [grid opt]
  (let [; we need to instantiate `vector` because core.typed's inference isn't good enough.
        ; Semantically these functions are exactly `vector`.
        row-states (inst vector AnyInteger (IPersistentVector State) Any Any Any Any)
        col-states (inst vector AnyInteger State Any Any Any Any)]
    (vec
      (for> :- (IPersistentVector State)
        [[row ss] :- '[AnyInteger (IPersistentVector State)] (map-indexed row-states grid)]
        ; row is the row number
        ; ss is a whole column of states
        (vec
          (for> :- State
            [[col s] :- '[AnyInteger State] (map-indexed col-states ss)]
            ; col is the col number
            ; s is a state at a point
            (next-state grid s [row col] opt)))))))

(ann update-simulation! [GnuplotP Grid -> nil])
(defn update-simulation!
  "Update the simulation with the provided grid"
  [{:keys [out] :as gp} grid]
  ; Print stdout straight to the Gnuplot process
  (binding [*out* out]
    (println 
      "plot '-' matrix using (0+1*$1):(-1-1*$2):3 title 'Forest Fire Simulation' with image")
    ; print each point. But first reverse the grid, we provide
    ; each row in reverse order.
    (doseq> [ss :- (Seqable State) (rseq grid)]
      (println
        (str/join " "
          (map state->number ss))))
    ; tell gnuplot we're done
    (println "e")
    (flush)))

(ann ^:nocheck clojure.core/slurp [Any -> String])

(ann setup-gnuplot! [GnuplotP -> nil])
(defn setup-gnuplot! 
  "Setup the Gnuplot window to prepare writing the simulation.
  Actual commands are in 'resources/setup-gnuplot'."
  [{:keys [out]}]
  ; Print stdout straight to the Gnuplot process
  (binding [*out* out]
    (println (slurp "resources/setup-gnuplot"))
    (flush)))

(ann run-simulation! [& {:p Number :f Number} -> nil])
(defn run-simulation!
  "Run a forest fire simulation via gnuplot."
  [& {:as opts}]
  (let [; start a new Gnuplot process
        gp (plot/start)
        ; get the initial world state
        s0 (initial-grid)]
    (setup-gnuplot! gp)
    (h/silent-hole)))

(comment
  (def current-proc (plot/start))
  (setup-gnuplot! current-proc)

  (def my-grid (atom (initial-grid)))
  (def my-opts {:p 0.5 :f 0.5})

  (defn my-next
    []
    (swap! my-grid next-grid my-opts)
    (update-simulation! current-proc @my-grid))

  (dotimes [_ 100] (my-next))

  (map identity [1 2 3]))
