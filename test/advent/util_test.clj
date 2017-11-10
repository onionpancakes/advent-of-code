(ns advent.util-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [advent.util :as util]))

(defspec parse-int-test
  (prop/for-all [i gen/int]
    (= i (util/parse-int (str i)))))

(defspec all-pos-abs-test
  (prop/for-all [i gen/int]
    (>= (util/abs i) 0)))

(defspec multiplicativity-abs-test
  (prop/for-all [a gen/int
                 b gen/int]
    (= (util/abs (* a b)) (* (util/abs a) (util/abs b)))))

(defspec triangle-inequality-abs-test
  (prop/for-all [a gen/int
                 b gen/int]
    (<= (util/abs (+ a b)) (+ (util/abs a) (util/abs b)))))
