(ns fire.simulate-test
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns ann typed-deps]]
            [fire.simulate :refer [Grid] :as sim]))

(defmacro deftest> [name & body]
  `(do (ann ~name ~'[-> Any])
       (deftest ~name ~@body)))

(typed-deps fire.simulate)

(ann ^:nocheck clojure.test/test-var [clojure.lang.Var -> Any])

(ann init-grid Grid)
(def init-grid (sim/initial-grid :rows 100, :cols 100))

(deftest> periodic-boundary-conditions-test
  (testing "Neighbours respect boundary conditions"
    (is (= (sim/neighbour-points init-grid [0 0])
           #{[0 1] [1 1] [1 0] [99 1] [99 0] [99 99] [0 99] [1 99]})
          )))
