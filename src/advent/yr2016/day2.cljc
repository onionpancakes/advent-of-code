(ns advent.yr2016.day2
  (:require [clojure.string :as cs]))

(def transitions-part1
  "State transitions for 3x3 numpad."
  {::up    {1 1, 2 2, 3 3,
            4 1, 5 2, 6 3,
            7 4, 8 5, 9 6}
   ::right {1 2, 2 3, 3 3,
            4 5, 5 6, 6 6,
            7 8, 8 9, 9 9}
   ::down  {1 4, 2 5, 3 6,
            4 7, 5 8, 6 9,
            7 7, 8 8, 9 9}
   ::left  {1 1, 2 1, 3 2,
            4 4, 5 4, 6 5,
            7 7, 8 7, 9 8}})

(def transitions-part2
  "State transitions for the diamond numpad."
  {::up    {          1 1,
                 2 2, 3 1, 4 4,
            5 5, 6 2, 7 3, 8 4, 9 9,
                :A 6 :B 7 :C 8,
                     :D :B}
   ::right {          1 1,
                 2 3, 3 4, 4 4,
            5 6, 6 7, 7 8, 8 9, 9 9,
               :A :B, :B :C, :C :C,
                      :D :D}
   ::down  {           1 3,
                  2 6, 3 7, 4 8,
            5 5, 6 :A, 7 :B, 8 :C, 9 9,
               :A :A, :B :D, :C :C,
                      :D :D}
   ::left  {          1 1,
                 2 2, 3 2, 4 3,
            5 5, 6 5, 7 6, 8 7, 9 8,
               :A :A, :B :A, :C :B,
                     :D :D}})

(def move-mapping
  {\U ::up
   \R ::right
   \D ::down
   \L ::left})

(defn compile-moves
  "Turn a move string a state transition function."
  [transitions moves]
  (transduce (comp (map move-mapping)
                   (map transitions))
             (completing #(comp %2 %1))
             identity
             moves))

(defn answer1
  [input]
  (->> (cs/split (cs/trim input) #"\n")
       (map (partial compile-moves
                     transitions-part1))
       (reductions #(%2 %1) 5)
       (rest)
       (map str)
       (cs/join "")))

(defn answer2
  [input]
  (->> (cs/split (cs/trim input) #"\n")
       (map (partial compile-moves
                     transitions-part2))
       (reductions #(%2 %1) 5)
       (rest)
       (map #(if (keyword? %)
               (name %)
               (str %)))
       (cs/join "")))

