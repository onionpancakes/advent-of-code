(ns advent.yr2016.day4-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :refer [resource]]
            [advent.yr2016.day4 :as day4]))

(deftest answers
  (let [input (slurp (resource "yr2016/day4/input.txt"))]
    (is (= "137896" (day4/answer1 input)))
    (is (= "501" (day4/answer2 input)))))


