(ns advent.yr2016.day2-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.java.io :refer [resource]]
            [clojure.string :as c.str]
            [advent.yr2016.day2 :as day2]))

;; Answers according to advent website.

(deftest answers
  (let [input (slurp (resource "yr2016/day2/input.txt"))]
    (is (= "78985" (day2/answer1 input)))
    (is (= "57DD8" (day2/answer2 input)))))

;; Unit tests

(deftest compile-moves-test
  (testing "basic left-right moves"
    (let [ts {::day2/right {1 2} ::day2/left {2 1}}]
      (are [start moves end] (= end ((day2/compile-moves ts moves) start))
        1 "" 1
        2 "" 2
        1 "R" 2
        2 "L" 1
        1 "RL" 1
        2 "LR" 2
        1 "RLR" 2
        2 "LRL" 1)))
  
  (testing "all move commands with a simple clockwise ring."
    (let [ts {::day2/up {4 1}
              ::day2/right {1 2}
              ::day2/down {2 3}
              ::day2/left {3 4}}]
      (are [start moves end] (= end ((day2/compile-moves ts moves) start))
        1 "R" 2
        1 "RD" 3
        1 "RDL" 4
        1 "RDLU" 1
        1 "RDLUR" 2))))

(deftest passcode-tokens-test
  (testing "basic left-right moves-list"
    (let [ts {::day2/right {1 2} ::day2/left {2 1}}]
      (are [start ml tokens] (= (day2/passcode-tokens ts start ml) tokens)
        1 [] []
        1 [[]] [1]
        1 [""] [1]
        1 ["R"] [2]
        1 ["RL"] [1]
        1 ["R" "L"] [2 1]
        1 ["RL" "RL"] [1 1]
        1 [(seq "RL") (seq "RL")] [1 1]
        1 ["R" "" "L"] [2 2 1]))))

;; Properties tests

(def move
  (gen/elements [\U \R \D \L]))

(def moves
  (gen/fmap (partial apply str) (gen/vector move)))

(def moves-list
  (gen/vector moves))

(defn identity-map
  [states-gen]
  (gen/fmap #(zipmap % %) states-gen))

(def directions
  [::day2/up ::day2/right ::day2/down ::day2/left])

(defn same-four-transitions
  "Use the same map for all four directions."
  [map-gen]
  (gen/fmap (comp (partial zipmap directions)
                  (juxt identity identity identity identity))
            map-gen))

(defn transitions-and-state
  "Return a 2-tuple [ts s], where ts is the transitions-map
  and s is a state within ts.

  Usually, s is used as an initial starting state."
  [transistions-gen]
  (gen/bind transistions-gen
            (fn [ts]
              (gen/tuple (gen/return ts)
                         (->> (vals ts)
                              (mapcat keys)
                              (into #{})
                              (gen/elements))))))

;; Apparently, Float/NaN != Float/NaN
(def self-equal-value
  (gen/such-that #(= % %) gen/any))

;; State applied to an identity transition should not change.
(defspec identity-moves
  (prop/for-all [[ts state] (transitions-and-state
                             (same-four-transitions
                              (identity-map
                               (gen/not-empty
                                (gen/vector self-equal-value)))))
                 ms moves]
    (= state ((day2/compile-moves ts ms) state))))

;; Passcodes generated from identity transistions
;; should just be repetitions of the initial starting state.
(defspec repeating-identity-passcode-tokens
  (prop/for-all [[ts state] (transitions-and-state
                             (same-four-transitions
                              (identity-map
                               (gen/not-empty
                                (gen/vector self-equal-value)))))
                 ml moves-list]
    (= (day2/passcode-tokens ts state ml)
       (repeat (count ml) state))))



(defn interval
  "Generator for 2-elements numeric vectors [a b] where a <= b."
  [num-gen]
  (gen/fmap (juxt (partial apply min) (partial apply max))
            (gen/tuple num-gen num-gen)))

(defn num-range
  [num-gen]
  (gen/fmap (partial apply range) (interval num-gen)))

(defn repeat-last
  [xs-gen]
  (->> (gen/fmap vec xs-gen)
       (gen/fmap #(conj % (peek %)))))

(defn sequential-map
  "Transition map where state transition in the same order
  as the generate sequence of states."
  [states-gen]
  (gen/fmap #(zipmap % (rest %)) states-gen))

;; Given a sequential map of ascending integer transitions,
;; passcode tokens should be monotonic.
(defspec monotonic-passcode-tokens
  (prop/for-all [[ts state] (transitions-and-state
                             (same-four-transitions
                              (sequential-map
                               (repeat-last
                                (gen/not-empty
                                 (num-range gen/int))))))
                 ml (gen/not-empty moves-list)]
    (apply <= (day2/passcode-tokens ts state ml))))

