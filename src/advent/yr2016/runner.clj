(ns advent.yr2016.runner
  (require [clojure.java.io :as io]
           [advent.yr2016.day1 :as day1]
           [advent.yr2016.day2 :as day2]
           [advent.yr2016.day4 :as day4]))

;; Day 1

(->> (io/resource "yr2016/day1/input.txt")
     (slurp)
     (day1/answer1)
     (println "Day1 Part1:"))

(->> (io/resource "yr2016/day1/input.txt")
     (slurp)
     (day1/answer2)
     (println "Day1 Part1:"))

;; Day 2

(->> (io/resource "yr2016/day2/input.txt")
     (slurp)
     (day2/answer1)
     (println "Day2 Part1:"))

(->> (io/resource "yr2016/day2/input.txt")
     (slurp)
     (day2/answer2)
     (println "Day2 Part2:"))

;; Day 4

(->> (io/resource "yr2016/day4/input.txt")
     (slurp)
     (day4/answer1)
     (println "Day4 Part1:"))

(->> (io/resource "yr2016/day4/input.txt")
     (slurp)
     (day4/answer2)
     (println "Day4 Part2:"))
