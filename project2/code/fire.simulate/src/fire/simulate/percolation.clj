(ns fire.simulate.percolation
  "This namespaces defines the initial grid function for
  the percolation problem"
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst into-array>
                                        override-method Atom1 letfn>]]
            [clojure.math.numeric-tower :refer [floor abs]]
            [clojure.tools.analyzer.hygienic :refer [macroexpand]]
            [clojure.reflect :as reflect]
            [fire.gnuplot :as plot]
            [fire.simulate :as sim]))

(typed-deps fire.simulate 
            fire.gnuplot)

;--------------------------------------------------
; Grid operations
;--------------------------------------------------

(ann initial-grid [& {:rows Long, :cols Long} :mandatory {:q Number} -> sim/Grid])
(defn initial-grid
  "Return the initial grid. States are :tree with probability q, and :empty
  with probability 1 - q.
  Ignites a small patch of trees in the centre of the grid.
  If not provided, number of rows and column default to 100."
  [& {:keys [rows cols q] :or {rows 100 cols 100}}]
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
                        (<= (abs (- row mid-row)) 2))))))

           gen-state :- [sim/Point -> sim/State]
           (gen-state
             ;State is a :tree with probability q, :empty
             ;with probability 1-q, or :burning if part of the
             ;small patch of trees in the centre of the grid.
             [p]
             (cond 
               (burning? p) :burning
               (sim/occurs? q) :tree 
               :else :empty))]
    (sim/grid-from-fn gen-state :rows rows, :cols cols)))

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
  (def proc (plot/start))
  (sim/setup-gnuplot! proc)
  (def state (initial-grid :q 0.1))
  )

(comment
  (A/getBasis)
  (extend-type A P (adsf [this] this))
  (extend-type nil P  (adsf [this] this))
  (do (defprotocol P
        (asdf [this]))
      (reflect/type-reflect (class (:adsf (get (:impls P) A)))
                            :reflector (clojure.reflect.JavaReflector. (.getContextClassLoader (Thread/currentThread))))))
(comment
  (A/getBasis)
  (do (defrecord A [a b c d])
      (reflect/type-reflect A 
                            :reflector (clojure.reflect.JavaReflector. (.getContextClassLoader (Thread/currentThread))))))
