(ns fire.simulate
  "This namespace is the main driver for the fire simulation."
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst
                                        letfn>]]
            [clojure.core.typed.hole :as h]
            [fire.gnuplot :as plot :refer [GnuplotP]]
            [clojure.string :as str])
  (:import (clojure.lang IPersistentVector IPersistentSet Seqable LazySeq)
           (java.io Writer)))

(typed-deps fire.gnuplot 
            fire.types
            clojure.core.typed.hole)

;-----------------------------------------------------------------
; Type Aliases
;-----------------------------------------------------------------

(def-alias State 
  "A point can either be empty, a tree, or a burning tree."
  (U ':burning ':tree ':empty))

(def-alias Grid
  "An immutable snapshot of the world state.
  
  - :grid   the grid
  - :rows   number of rows
  - :cols   number of columns"
  '{:grid (IPersistentVector (IPersistentVector State))
    :rows AnyInteger,
    :cols AnyInteger
    :history (IPersistentVector '{:nburning AnyInteger})})

(def-alias Point 
  "A point in 2d space."
  '[AnyInteger, AnyInteger])

(def-alias GridOpt 
  "Options to configure grid generation.
  :p - the probability a tree will grow in an empty site
  :f - the probability a site with a tree will burn (lightning)"
  '{:p Number,
    :f Number})

;-----------------------------------------------------------------
; Utility functions
;-----------------------------------------------------------------

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


;-----------------------------------------------------------------
; Grid operations
;-----------------------------------------------------------------

(ann grid-from-fn [[Point -> State] & {:rows Long, :cols Long} -> Grid])
(defn grid-from-fn 
  "Generate a grid with dimensions rows by cols. state-fn
  is fed each Point in the grid, and should return the initial state
  at that point."
  [state-fn & {:keys [rows cols] :or {rows 100 cols 100}}]
  {:grid (vec
           (for> :- (IPersistentVector State)
                 [row :- AnyInteger, (range rows)]
                 (vec
                   (for> :- State
                         [col :- AnyInteger, (range cols)]
                         (state-fn [row col])))))
   :rows rows
   :cols cols
   :history []})

(ann initial-grid [& {:rows Long, :cols Long} -> Grid])
(defn initial-grid
  "Return the initial grid state, a vector of vectors, with each
  position initialised to :empty. If not provided, number of rows and column default
  to 100."
  [& {:keys [rows cols] :or {rows 100 cols 100}}]
  (grid-from-fn (constantly :empty) :rows rows :cols cols))

(ann state-at [Grid Point -> State])
(defn state-at 
  "Return the state in the provided grid, at the provided 2d point.
  Throws an exception if the point is outside the grid's dimensions."
  [grid [row col :as pnt]]
  (-> (:grid grid)
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

(ann grid-dimensions [Grid -> '{:nrows AnyInteger, :ncols AnyInteger}])
(defn grid-dimensions [grid]
  {:nrows (:rows grid)
   :ncols (:cols grid)})

(ann neighbour-points [Grid Point -> (LazySeq Point)])
(defn neighbour-points 
  "Return the a lazy sequence containing the states of neighbour points of point pnt, respecting
  periodic boundary conditions"
  [grid [^long row, ^long col, :as p]]
  (letfn> [wrap-around :- [AnyInteger AnyInteger -> AnyInteger]
           ; takes a magnitude of a dimension and the length of the
           ; dimension and corrects it for periodic boundary conditions
           (wrap-around [^long new-x ^long x-length]
             (cond
               ; wrap up
               (< new-x 0) (+ new-x x-length)
               ; wrap down
               (>= new-x x-length) (- new-x x-length)
               ; already inside
               :else new-x))]
    (let [{:keys [nrows ncols]} (grid-dimensions grid)
          ; check current point is between grid bounds
          _ (assert (<= 0 row (dec nrows)) "Row out of bounds")
          _ (assert (<= 0 col (dec ncols)) "Column out of bounds")]
      ; collect the states of each neighbour, respecting periodic boundary conditions
      (for> :- Point 
            [[^long row-diff ^long col-diff] :- '[Long Long], neighbour-positions]
            (let [neighbour-row (wrap-around (+ row row-diff) nrows)
                  neighbour-col (wrap-around (+ col col-diff) ncols)]
              [neighbour-row neighbour-col])))))

(ann nearest-neighbours [Grid Point -> (LazySeq State)])
(defn nearest-neighbours 
  "Return a lazy sequence containing the states of the nearest neighbours
  of point p0 in grid. Respects periodic boundary conditions."
  [grid p0]
  (->> (neighbour-points grid p0)
       (map (fn> [p1 :- Point]
              (state-at grid p1)))))

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
      (if (some #(= :burning %) neighbours)
        :burning
        (if (occurs? f) 
          :burning
          :tree))

      ;3. At an empty site, a tree grows with probability p
      :empty (if (occurs? p)
               :tree
               :empty))))

(ann flat-grid [Grid -> (Seqable State)])
(defn flat-grid [{:keys [grid]}]
  (ann-form grid (Seqable (Seqable State)))
  (apply concat grid))

(ann next-grid [Grid GridOpt -> Grid])
(defn next-grid 
  "Simultaneously update a Grid to the next time increment
  according to the 4 update rules. Observed periodic boundary conditions.
  See next-state for the state increment."
  [grid opt]
  (let [; ---------------------------------------------------------------------------------
        ; START TYPE SYSTEM BOILERPLATE
        ;  we need to instantiate `vector` because core.typed's inference isn't good enough.
        ;  Semantically both row-states and col-states are exactly clojure.core/vector.
        row-states (inst vector AnyInteger (IPersistentVector State) Any Any Any Any)
        col-states (inst vector AnyInteger State Any Any Any Any)
        ; END BOILERPLATE
        ;----------------------------------------------------------------------------------
        ]
    (-> grid
      (assoc :history 
             (conj (:history grid) {:nburning (count (filter #(= :burning %) (flat-grid grid)))}))
      (assoc :grid
             (vec
               (for> :- (IPersistentVector State)
                 [[row ss] :- '[AnyInteger (IPersistentVector State)], (map-indexed row-states (:grid grid))]
                 ; row is the row number
                 ; ss is a whole column of states
                 (vec
                   (for> :- State
                     [[col s] :- '[AnyInteger State], (map-indexed col-states ss)]
                     ; col is the col number
                     ; s is a state at a point
                     (next-state grid s [row col] opt)))))))))


;-----------------------------------------------------------------
; gnuplot operations
;-----------------------------------------------------------------

(ann update-simulation! [GnuplotP Grid & {} :mandatory {:time-code Number} -> nil])
(defn update-simulation!
  "Update the simulation with the provided grid.

  Accepts mandatory keyword parameters:
    - :time-code  an integer of the current frame number"
  [{:keys [out] :as gp} {:keys [history] :as grid} & {:keys [time-code] :as opt}]
  (ann-form history (IPersistentVector '{:nburning AnyInteger}))
  (let [{:keys [nrows ncols]} (grid-dimensions grid)]
    ; *out* is gnuplot
    (binding [*out* out]
      ;plot the forest
      (println (slurp "resources/setup-gnuplot.gpi"))
      (println "set term x11 0")
      (println "unset grid")
      (println "unset xlabel")
      (println "unset ylabel")
      (println "unset xtics")
      (println "unset ytics")
      (println "unset x2tics")
      (println "unset y2tics")
      (println
        (str "plot '-' binary array=" nrows  "x" ncols
             " flipy format='%char' title 'Forest Fire Simulation - Frame " time-code
             "' with image"))
      ; print each point to gnuplot as a char array.
      (let [^chars arr (char-array 
                         ; array length is rowsxcols
                         (* nrows ncols)
                         (map (fn> [s :- State]
                                (-> s state->number char))
                              ; reverse the grid, we provide each row in reverse order.
                              (apply concat (rseq (:grid grid)))))]
        (.write ^Writer *out* arr))
      ;plot the burning tree graph
      (println "set term x11 1")
      (println "unset grid")
      (println "set title 'Number of Burning trees'")
      (println "set xlabel 'Time'")
      (println "set ylabel 'Number of Burning trees'")
      ;plot the last 20 entries
      (println (str "plot [" (max (- (count history) 20) 0) ":" (count history) "][0:] '-' with lines"))
      (doseq> [{:keys [nburning]} :- '{:nburning AnyInteger}, history]
        (println nburning))
      ; tell gnuplot we're done
      (println)
      (println "e")
      (flush))))

(ann setup-gnuplot! [GnuplotP -> nil])
(defn setup-gnuplot! 
  "Setup the gnuplot window to prepare writing the simulation.
  Actual setup commands are in 'resources/setup-gnuplot.gpi'."
  [{:keys [out]}]
  ; Print stdout straight to the Gnuplot process
  (binding [*out* out]
    (println (slurp "resources/setup-gnuplot.gpi"))
    (flush)))


(comment
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
  (def current-proc (plot/start))
  (setup-gnuplot! current-proc)

  (def my-grid (atom (initial-grid)))
  (def frame-number (atom 0))
  (def my-opts {:p 0.1 :f 0.1})

  (defn my-next []
    (swap! my-grid next-grid my-opts)
    (swap! frame-number inc)
    (update-simulation! current-proc @my-grid {:time-code @frame-number}))

  (dotimes [_ 100] (my-next))

  (map identity [1 2 3]))
