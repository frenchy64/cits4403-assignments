(ns fire.gnuplot
  (:require [clojure.core.typed :refer [ann check-ns def-alias non-nil-return print-env cf]])
  (:import (java.io BufferedWriter OutputStreamWriter Writer)))

; start and stop stolen from https://bitbucket.org/vshender/clojure-gnuplot/src/bcff38db8260059fd7114ccf202237060a472278/src/org/shender/clojure_gnuplot.clj?at=default

; A gnuplot process
(def-alias GnuplotP '{:proc Process, :out Writer})

; We know these method never return null.
(non-nil-return java.lang.Runtime/getRuntime :all)
(non-nil-return java.lang.Process/getOutputStream :all)
(non-nil-return java.lang.Runtime/exec :all)

(ann start [-> GnuplotP])
(defn start
  "Start gnuplot process."
  []
  (let [^Process proc (.exec (Runtime/getRuntime) "gnuplot")
        out  (-> (.getOutputStream proc) OutputStreamWriter. BufferedWriter.)]
    {:proc proc :out out}))

(ann stop [GnuplotP -> Any])
(defn stop
  "Stop gnuplot process."
  [gnuplot]
  (.destroy ^Process (:proc gnuplot)))

