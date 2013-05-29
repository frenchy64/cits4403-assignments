(ns fire.simulate.percolation
  "This namespaces defines the initial grid function for
  the percolation problem"
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed :refer [ann check-ns typed-deps def-alias ann-datatype
                                        for> fn> ann-form AnyInteger doseq> cf inst into-array>
                                        override-method Atom1 letfn> ann-form dotimes>
                                        nilable-param non-nil-return]
             :as tc]
            [clojure.math.numeric-tower :refer [floor abs]]
            [clojure.tools.analyzer.hygienic :refer [macroexpand]]
            [clojure.reflect :as reflect]
            [fire.gnuplot :as plot]
            [fire.simulate :as sim]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.trace :as trace])
  (:import (clojure.lang IPersistentVector Seqable)))

(typed-deps fire.simulate 
            fire.gnuplot)

;--------------------------------------------------
; Types
;--------------------------------------------------

(def-alias PercolationP 
  "The state of a percolation run.

  - :gnuplot  the actual gnuplot process
  - :grid     a grid used to update gnuplot
  - :q        the initial probability of a green tree at time 0
  - :p        the probability of tree growth at every time interval
  - :f        the probability of lightning
  - :frame    the frame number corresponding to the grid, displayed in the gnuplot graph"
  '{:gnuplot plot/GnuplotP,
    :grid sim/Grid,
    :q Number
    :p Number
    :f Number
    :frame AnyInteger})

;--------------------------------------------------
; Grid operations
;--------------------------------------------------

(ann initial-grid [& {:rows Long, :cols Long :q Number} -> sim/Grid])
(defn initial-grid
  "Return the initial grid. States are :tree (green tree) with probability q, and :empty
  (empty point) with probability 1 - q.
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

(ann start! [& {:q Number, :grid sim/Grid, :p Number, :f Number :no-update Any, :gnuplot plot/GnuplotP} -> PercolationP])
(defn start! 
  "Start a gnuplot process and initialize it to the starting
  state. 

  Keyword arguments:
  - :grid  an optional initial grid instead of generating one
  - :q     the :q passed to initial-grid.
  - :p     the probability of a tree growing in an empty square.
  - :f     the probability of lightning.
  - :no-update  If true, do not update a graph with the initial state. Defaults to nil.
  - :gnuplot  an optional gnuplot process

  Returns a map for percolation ops."
  [& {:keys [q p f grid no-update gnuplot] :or {q 0.1, p 0, f 0}}]
  (let [grid0 (or grid (initial-grid :q q))
        proc (or gnuplot (plot/start))
        frame0 0]
    (sim/with-gunplot-toplevel proc
      (sim/setup-gnuplot! proc)
      (when-not no-update
        (sim/update-simulation! proc grid0 :time-code frame0)))
    {:gnuplot proc,
     :grid grid0,
     :q q,
     :p p,
     :f f,
     :frame frame0}))

(ann next! [PercolationP & {:no-update Any} -> PercolationP])
(defn next!
  "Calculate the next state for the percolation plot, and update
  the gnuplot process. Returns a new map for percolation ops.
  
  Options
  - :no-update  If true, do not plot the next grid. Defaults to nil."
  [{grid0 :grid, frame0 :frame, :keys [p f gnuplot] :as perc} & {:keys [no-update]}]
  (let [grid1 (sim/next-grid grid0 {:p p, :f f})
        frame1 (inc frame0)]
    (when-not no-update
      (sim/with-gunplot-toplevel gnuplot
        (sim/update-simulation! gnuplot grid1 :time-code frame1)))
    (-> perc
        (assoc :grid grid1)
        (assoc :frame frame1))))

(ann set-eps String)
(def set-eps "set terminal postscript eps enhanced")

(ann set-epslatex String)
(def set-epslatex "set terminal epslatex")

(ann flush-epslatex [-> nil])
(defn flush-epslatex 
  "Sometimes we need to change the output terminal to
  help epslatex output a full tex file. This makes
  'output' and 'terminal' dirty, they should be set again."
  []
  (println "set output")
  (flush))

(ann plot-forest-to-eps [PercolationP String -> nil])
(defn plot-forest-to-eps
  "Plot a forest grid to eps, to the specified path (extension omitted)"
  [{:keys [gnuplot grid frame]} path-no-ext]
  (println set-epslatex)
  (println (str "set output '" path-no-ext ".tex'"))
  (sim/plot-forest gnuplot grid frame)
  (flush-epslatex))

(ann ^:nocheck clojure.core/replace (All [x y] [(clojure.lang.IPersistentMap Any y) (clojure.lang.Seqable y) 
                                                -> (clojure.lang.Seqable y)]))

(ann update-without-plot (Fn [PercolationP -> PercolationP]
                             [PercolationP AnyInteger -> PercolationP]))
(defn- update-without-plot
  ([p] (update-without-plot p 1))
  ([p n]
   {:pre [(integer? n)]}
   (if (zero? n)
     p
     (recur (next! p :no-update true) (dec n)))))

(ann run-percolation-test [String AnyInteger -> nil])
(defn run-percolation-test
  "Takes a string naming the output folder, and the
  value of q, and outputs the graphs needed for the percolation test"
  [folder q]
  (letfn> [update-without-plot :- [PercolationP -> PercolationP]
           (update-without-plot [p]
             (next! p :no-update true))

           path-no-ext :- [String -> String]
           (path-no-ext [s]
             (str s "-q" (apply str (replace {\. \-} (str q)))))]
    (let [; calculate the initial grid
          initial-state (start! :q q :p 0 :f 0 :no-update true)

          ; calculate the final grid after 100 interations
          {final-grid :grid,
           :as final-state} (last (take 100 (iterate update-without-plot initial-state)))
          _ (assert final-state)
          _ (assert final-grid)

          gp (:gnuplot final-state)]
      (sim/with-gunplot-toplevel gp

        ;; output initial graph
        (let [nme-no-ext (path-no-ext "initial-state")]
          (plot-forest-to-eps initial-state nme-no-ext)
          (flush-epslatex))

        ;; output nburning graph
        (do
          (println set-epslatex "color" "size 3,3")
          (println (str "set output '" (path-no-ext "nburning") ".tex'"))
          (sim/plot-nburning-graph gp final-grid)
          (flush-epslatex))

        ;; output ntree graph
        (do
          (println set-epslatex "color" "size 3,3")
          (println (str "set output '" (path-no-ext "ntree") ".tex'"))
          (sim/plot-ntrees-graph gp final-grid)
          (flush-epslatex))))))

(ann never-nil (All [x] [(U x nil) -> x]))
(defn never-nil [a]
  (assert (not (nil? a)) "Found nil")
  a)

(ann ^:nocheck clojure.core/spit [clojure.java.io/IOFactory Any & {:append Any} -> Any])

(nilable-param java.io.File/createTempFile {2 #{1}})
(non-nil-return java.io.File/createTempFile :all)

(ann multi-plot-nburning-percolation [plot/GnuplotP (Seqable PercolationP) -> nil])
(defn multi-plot-nburning-percolation 
  "Make a nice multiplot to study relationship of q and grid size
  to fire percolation."
  [gp ps]
  (assert (seq ps) "Must provide at least one grid")
  (let [get-history (fn> [p :- PercolationP] 
                         (-> p :grid :history 
                             (ann-form sim/GridHistory)
                             count))]
    (assert (apply == (get-history (first ps))
                   (map get-history (next ps)))
            "All grid histories must be identical length"))
  (let [temp-file (java.io.File/createTempFile "multiplot-data" nil)
        history-count (-> ps first :grid :history 
                          (ann-form sim/GridHistory)
                          count)
        histories (map (fn> :- sim/GridHistory
                            [p :- PercolationP]
                            (-> p :grid :history))
                       ps)]
    (prn temp-file)
    ;dump data to a temp file
    (dotimes> [n history-count]
      (let [nburnings (map (fn> :- Number
                             [e :- sim/GridHistory]
                             (-> (nth e n) 
                                 (ann-form sim/GridHistoryEntry)
                                 :nburning
                                 (ann-form Number)))
                           histories)]
        (spit temp-file (str (str/join " " (cons n nburnings)) "\n") :append true)))
      (sim/with-gunplot-toplevel gp
        ;; plot nburning graph
          (println set-epslatex " color")
          (println "set output 'multi-nburning.tex'")
          (println "set title 'Number of Burning trees'")
          (println "set xlabel 'Time'")
          (println "set ylabel 'Burning trees'")
          (println "set xtics nomirror autofreq")
          (println "set ytics nomirror autofreq")
          (println "unset x2tics")
          (println "unset y2tics")
          (println "set key outside")
          (println "set key right top")
          (println "save 'multigraph.gp'")
          (let [plot-opts (str/join ", '' " ; '' is reusing the output (temp file) from previous calls
                           (let [indexed-ps (map-indexed (inst vector Number PercolationP Any Any Any Any) ps)]
                             (for> :- String
                               [[n {{:keys [rows cols]} :grid, :keys [q style]}] :- '[Number PercolationP], indexed-ps]
                               (do 
                                 (ann-form [q rows cols] '[Number Number Number])
                                 (assert (number? q))
                                 (assert (number? rows))
                                 (assert (number? cols))
                                 (str "using 1:" (+ 2 n)
                                      " title 'q=" q
                                      ", " rows "x" cols "'"
                                      )))))]
            (binding [*out* (io/writer (never-nil System/out))]
              (prn (with-out-str (println (str "plot '" (.getAbsolutePath temp-file) "'") plot-opts))))
            (println (str "plot '" (.getAbsolutePath temp-file) "'") plot-opts))
          (sim/gnuplot-eof gp)
          (flush-epslatex)
          (flush))))

(def-alias MultiPlotEntry
  "Internal to do-multiplot-task.
  
  - :size  make a grid with dimensions size x size
  - :q     probably of points being a tree is q"
  '{:size Long, :q Number})

(ann do-multiplot-task [-> nil])
(defn do-multiplot-task
  "Make the plot for percolation task using several different grids"
  []
  (let [gp (plot/start)]
    (letfn> [start :- [& {} :mandatory {:size Long, :q Number} -> PercolationP]
             (start [& {:keys [size q]}]
               (start! :q q :grid (initial-grid :q q :rows size :cols size)
                       :gnuplot gp :no-update true))

             make-grids :- [Number MultiPlotEntry * -> (Seqable PercolationP)]
             (make-grids [iters & specs]
               (for> :- PercolationP
                 [{:keys [size q]} :- MultiPlotEntry, specs]
                 (-> (start :size size :q q)
                     (update-without-plot 100))))]
      (let [gs (make-grids
                 100
                 {:size 50, :q 0.3}
                 {:size 50, :q 0.4}
                 {:size 50, :q 0.5}
                 {:size 100, :q 0.3}
                 {:size 100, :q 0.4}
                 {:size 100, :q 0.5}
                 {:size 200, :q 0.3}
                 {:size 200, :q 0.4}
                 {:size 200, :q 0.5})]
        (multi-plot-nburning-percolation gp gs)))))

(comment
(ann spit-grid [Grid IOFactory])
(defn spit-grid
  [grid path]
  (spit path (with-out-str
               (doall
               (for [row (rseq (:grid grid))]
                 (println (apply str (interpose \  (map sim/state->number row))))))
               (flush))))
  )

;; scratch below

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
(def proc (atom (start! :p 0 :f 0
                        :grid (initial-grid :q 0.39 :rows 200 :cols 200))))
  (spit-grid (:grid @proc) "outhere")

  (dotimes [_ 100]
    (swap! proc next!))

  (output-latex-graph @proc "foobar")
  (run-percolation-test "." 0.5)
  (run-percolation-test "." 0.3)
  (run-percolation-test "." 0.4)
  (do-multiplot-task)
  )

 
