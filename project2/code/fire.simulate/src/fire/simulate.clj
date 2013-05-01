; This namespace is the main driver for the fire simulation.

(ns fire.simulate
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger]]
            [fire.gnuplot :as plot])
  (:import (clojure.lang IPersistentVector IPersistentSet)))

;---------------------------
;Holes
(ann hole [-> Nothing])
(defn hole [] 
  (throw (Exception. "hole")))
(ann-datatype Hole [])
(deftype Hole [])
;End Holes
;---------------------------

; Type check these namespaces before the current one.
(typed-deps fire.gnuplot)

; # Define some type aliases
; - Grid is a vector of vectors with leafs either the keyword :burning, :tree or :empty.
(def-alias State (U ':burning ':tree ':empty))
(def-alias Grid (IPersistentVector (IPersistentVector State)))
(def-alias Point '[AnyInteger AnyInteger])

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
  ;coordinates start lower left
  ; ul u ur
  ; l    r
  ; ll l lr
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
                                 [[[row-diff col-diff] :- '[Long Long]] neighbour-positions
                                  ;filter out points outside the boundary
                                  :when (and (<= 0 (+ row row-diff) upper-row)
                                             (<= 0 (+ col col-diff) upper-col))]
                             (state-at grid [(+ row row-diff) (+ col col-diff)])))]
    neighbour-states))

(ann ^:nocheck clojure.core/range
     (Fn [-> (clojure.lang.LazySeq AnyInteger)]
         [Number -> (clojure.lang.LazySeq AnyInteger)]
         [AnyInteger Number -> (clojure.lang.LazySeq AnyInteger)]
         [Number Number -> (clojure.lang.LazySeq Number)]
         [AnyInteger Number AnyInteger -> (clojure.lang.LazySeq AnyInteger)]
         [Number Number Number -> (clojure.lang.LazySeq Number)]))

(ann next-state [Grid -> Grid])
(defn next-state 
  "Simultaneously update a Grid to the next time increment
  according to the 4 update rules."
  [grid]
  (vec
    (for> :- (IPersistentVector State)
          [[[row ss] :- '[AnyInteger (IPersistentVector State)]] 
           ;FIXME core.typed has trouble with map-indexed
           (map (ann-form #(vector %1 %2)
                          [AnyInteger (IPersistentVector State)
                           -> '[AnyInteger (IPersistentVector State)]])
                (range) grid)]
      (vec
        (for> :- State
              [[[col s] :- '[AnyInteger State]] (map (ann-form #(vector %1 %2)
                                                               [AnyInteger State -> '[AnyInteger State]])
                                                     (range) ss)]
              ; here we're updating point (row,col)
              (let [neighbours (nearest-neighbours grid [row col])]
                (case s
                  ;1. A burning tree becomes an empty site
                  :burning :empty

                  ;2. A viable tree becomes a burning tree if at least one of its nearest neighbours
                  ;is burning
                  ;4. A tree without a burning nearest neighbour becomes a burning tree during
                  ;one time step with probability f (e.g. lightning).
                  ;TODO make f parameter, atm f = 0
                  :tree 
                  (if (:burning neighbours)
                    :burning
                    :tree)

                  ;TODO make p parameter, atm p = 1
                  ;3. At an empty site, a tree grows with probability p
                  :empty :tree)))))))

(ann run-simulation [& {} -> nil])
(defn run-simulation 
  "Run a forest fire simulation via gnuplot."
  [& {:as opts}]
  (let [; start a new Gnuplot process
        ;  - out is the OutputStream of the process
        ;  - proc is the Process
        {:keys [proc out]} (plot/start)]
  nil))

(comment
  (map identity [1 2 3]))
