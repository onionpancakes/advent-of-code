(ns advent.yr2016.day1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :refer [resource]]
            [advent.yr2016.day1 :as day1]))

(deftest answers
  (let [input (slurp (resource "yr2016/day1/input.txt"))]
    (is (= "230" (day1/answer1 input)))
    (is (= "154" (day1/answer2 input)))))


