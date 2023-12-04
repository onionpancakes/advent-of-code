(ns dev.onionpancakes.adventofcode.2023.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (io/resource "2023/day2/input.txt"))

(defn parse-token
  [line]
  (let [groups (re-matches #"(\d+) (red|green|blue)" line)
        num    (-> (nth groups 1) (parse-long))
        color  (-> (nth groups 2) (keyword))]
    [color num]))

(defn parse-round
  [line]
  (->> (s/split line #", ")
       (map parse-token)
       (into {})))

(defn parse-rounds
  [line]
  (->> (s/split line #"; ")
       (map parse-round)
       (into [])))

(defn parse-game
  [line]
  (let [game-id (-> (re-find #"Game (\d+)" line)
                    (nth 1)
                    (parse-long))
        idx     (s/index-of line ": ")
        rounds  (subs line (+ idx 2))]
    {:game-id game-id
     :rounds  (parse-rounds rounds)}))

(defn parse-input
  []
  (->> (slurp input)
       (s/split-lines)
       (mapv parse-game)))

;; Part 1

(defn possible-round?
  [{:keys [red green blue]}]
  (cond
    (and (some? red) (> red 12))     false
    (and (some? green) (> green 13)) false
    (and (some? blue)(> blue 14))    false
    :default                         true))

(defn possible-game?
  [{:keys [rounds]}]
  (every? possible-round? rounds))

(defn answer-part1
  []
  (->> (parse-input)
       (filter possible-game?)
       (map :game-id)
       (reduce + 0)))

;; Part 2

(defn game-min-set-power
  [{:keys [rounds]}]
  (let [{:keys [red green blue]
         :or   {red 0 green 0 blue 0}} (apply merge-with max rounds)]
    (* red green blue)))

(defn answer-part2
  []
  (->> (parse-input)
       (map game-min-set-power)
       (reduce + 0)))
