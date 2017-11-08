(ns advent.yr2016.day2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :refer [resource]]
            [advent.yr2016.day2 :as day2]))

(deftest answers
  (let [input (slurp (resource "yr2016/day2/input.txt"))]
    (is (= "78985" (day2/answer1 input)))
    (is (= "57DD8" (day2/answer2 input)))))


