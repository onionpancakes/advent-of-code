(ns dev.onionpancakes.adventofcode.2023.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (io/resource "2023/day1/input.txt"))

;; Part 1

(defn digit-char?
  [ch]
  (case ch
    (\1 \2 \3 \4 \5 \6 \7 \8 \9) true
    false))

(defn calibration-value
  [line]
  (let [first-digit (->> (drop-while (complement digit-char?) line)
                         (first))
        last-digit  (->> (reverse line)
                         (drop-while (complement digit-char?))
                         (first))]
    (parse-long (str first-digit last-digit))))

(defn answer-part1
  []
  (->> (slurp input)
       (s/split-lines)
       (map calibration-value)
       (reduce + 0)))

;; Part 2

(def re-forward-digit
  #"zero|one|two|three|four|five|six|seven|eight|nine|[0-9]")

(def re-reverse-digit
  #"orez|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9]")

(def to-number-str
  {"zero"  "0"
   "orez"  "0"
   "one"   "1"
   "eno"   "1"
   "two"   "2"
   "owt"   "2"
   "three" "3"
   "eerht" "3"
   "four"  "4"
   "ruof"  "4"
   "five"  "5"
   "evif"  "5"
   "six"   "6"
   "xis"   "6"
   "seven" "7"
   "neves" "7"
   "eight" "8"
   "thgie" "8"
   "nine"  "9"
   "enin"  "9"
   "0"     "0"
   "1"     "1"
   "2"     "2"
   "3"     "3"
   "4"     "4"
   "5"     "5"
   "6"     "6"
   "7"     "7"
   "8"     "8"
   "9"     "9"})

(defn calibration-value-part2
  [line]
  (let [first-digit (re-find re-forward-digit line)
        last-digit  (re-find re-reverse-digit (apply str (reverse line)))]
    (parse-long (str (to-number-str first-digit)
                     (to-number-str last-digit)))))

(defn answer-part2
  []
  (->> (slurp input)
       (s/split-lines)
       (map calibration-value-part2)
       (reduce + 0)))
