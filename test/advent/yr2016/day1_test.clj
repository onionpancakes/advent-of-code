(ns advent.yr2016.day1-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.java.io :refer [resource]]
            [clojure.core.matrix :as cm]
            [advent.yr2016.day1 :as day1]
            [advent.util :as util]))

;; Answers according to advent website

(deftest answers
  (let [input (slurp (resource "yr2016/day1/input.txt"))]
    (is (= "230" (day1/answer1 input)))
    (is (= "154" (day1/answer2 input)))))

;; Unit tests

(deftest parse-direction-test
  (are [r s] (= r (day1/parse-direction s))
    [:R 0] "R0"
    [:R 1] "R1"
    [:R 1] "R01"
    [:L 1] "L1"
    [:L 123] "L123"))

(deftest direction-matrix-test
  (are [r s] (cm/e== r (day1/direction-matrix s))
    [[1 0 0 1]
     [0 1 -1 0]
     [0 0 0 1]
     [0 0 -1 0]] [:R 1]
    [[1 0 0 2]
     [0 1 -2 0]
     [0 0 0 1]
     [0 0 -1 0]] [:R 2]
    [[1 0 0 -1]
     [0 1 1 0]
     [0 0 0 -1]
     [0 0 1 0]] [:L 1]
    [[1 0 0 -2]
     [0 1 2 0]
     [0 0 0 -1]
     [0 0 1 0]] [:L 2]))

(deftest travel-test
  (are [r init dirs] (cm/e== r (day1/travel init dirs))
    [0 0 0 1] [0 0 0 1] []
    [1 0 1 0] [0 0 0 1] [[:R 1]]
    [1 1 0 1] [0 0 0 1] [[:R 1] [:L 1]]
    [4 3 0 1] [0 0 0 1] [[:R 4] [:L 3]]
    [0 0 0 1] [0 0 0 1] [[:R 1] [:R 1] [:R 1] [:R 1]]
    [-1 -1 0 -1] [0 0 0 1] [[:L 1] [:L 1]]
    [-3 -4 0 -1] [0 0 0 1] [[:L 3] [:L 4]]
    [2 2 0 1] [1 1 0 1] [[:R 1] [:L 1]]
    [0 0 0 -1] [1 1 0 -1] [[:R 1] [:L 1]]
    [2 0 1 0] [1 1 1 0] [[:R 1] [:L 1]]
    [0 2 -1 0] [1 1 -1 0] [[:R 1] [:L 1]]))

(deftest int-coords-test
  (are [r v] (= r (day1/int-coords v))
    [0 0] [0 0 0 0]
    [0 0] [0.0 0.0 0.0 0.0]
    [-2 3] [-2.0 3.0 0 0]))

(deftest distance-test
  (are [r a b] (= r (day1/distance a b))
    0 [0 0] [0 0]
    1 [0 0] [0 1]
    1 [0 0] [1 0]
    2 [0 0] [1 1]
    2 [0 0] [-1 -1]
    2 [1 1] [2 2]
    2 [1 1] [2 0]
    2 [-1 -1] [-2 -2]
    7 [0 0] [4 3]))

(deftest segment-steps-test
  (are [r s] (= r (day1/segment-steps s))
    [[0 0]] [[0 0] [0 0]]
    [[1 1]] [[1 1] [1 1]]
    ;; forward horizontal and vertical
    [[0 0] [0 1] [0 2]] [[0 0] [0 2]]
    [[0 0] [1 0] [2 0]] [[0 0] [2 0]]
    ;; backwards
    [[0 2] [0 1] [0 0]] [[0 2] [0 0]]
    [[2 0] [1 0] [0 0]] [[2 0] [0 0]]
    ;; forwards, crossing origin
    [[0 -1] [0 0] [0 1]] [[0 -1] [0 1]]
    [[-1 0] [0 0] [1 0]] [[-1 0] [1 0]]
    ;; backwards, crossing origin
    [[0 1] [0 0] [0 -1]] [[0 1] [0 -1]]
    [[1 0] [0 0] [-1 0]] [[1 0] [-1 0]]))

(deftest first-duplicate-test
  (are [r xs] (= r (day1/first-duplicate xs))
    nil []
    nil [0]
    nil [0 1 2 3]
    0 [0 0 1 2]
    1 [0 1 1 2]
    2 [0 1 2 2]
    0 [0 1 4 2 0 4 2 8]
    :foo [0 1 :foo 8 "foo" :foo {} 0]))

(deftest travel-stops-test
  (are [r init dirs] (->> (map vector r (day1/travel-stops init dirs))
                          (every? (partial apply cm/e==)))
    [[0 0 0 1]] [0 0 0 1] []
    [[0 0 0 1]
     [1 0 1 0]] [0 0 0 1] [[:R 1]]
    [[0 0 0 1]
     [1 0 1 0]
     [1 1 0 1]] [0 0 0 1] [[:R 1] [:L 1]]
    [[0 0 0 1]
     [-1 0 -1 0]
     [-1 1 0 1]] [0 0 0 1] [[:L 1] [:R 1]]
    [[1 1 0 1]
     [2 1 1 0]
     [2 2 0 1]] [1 1 0 1] [[:R 1] [:L 1]]))

(deftest trail-test
  (are [r coords] (= r (day1/trail coords))
    [] []
    [] [[0 0]]
    [[0 0]
     [0 1]] [[0 0] [0 1]]
    [[0 0]
     [0 1]
     [0 2]] [[0 0] [0 2]]
    [[0 0]
     [0 -1]
     [0 -2]] [[0 0] [0 -2]]
    [[0 0]
     [0 1]
     [0 2]
     [-1 2]
     [-2 2]] [[0 0] [0 2] [-2 2]]))

(deftest first-intersect
  (are [r init dirs] (= r (day1/first-intersect init dirs))
    nil [0 0 0 1] []
    [0 0] [0 0 0 1] [[:R 1] [:R 1] [:R 1] [:R 1]]
    [1 0] [0 0 0 1] [[:R 2] [:R 1] [:R 1] [:R 2]]
    [1 0] [0 0 0 1] [[:R 2] [:R 1] [:R 1] [:R 3]
                     [:R 1] [:R 1] [:R 1] [:R 1]]))

;; Properties test

(def position
  "Random position vector."
  (-> (gen/elements [[0 1] [1 0] [0 -1] [-1 0]])
      (gen/bind (fn [[dx dy]]
                  (gen/tuple gen/int gen/int (gen/return dx) (gen/return dy))))))

;; Prop test matrices

;; Traveling forward should not change orientation.
(defspec forward-matrix-perserves-orientation
  (prop/for-all [[_ _ dx dy :as pos] position
                 n gen/pos-int]
    (let [[_ _ fdx fdy] (cm/mmul (day1/forward n) pos)]
      (and (== fdx dx) (== fdy dy)))))

;; Traveling forward by n steps should increase
;; distance by n.
(defspec forward-matrix-distance
  (prop/for-all [[x y _ _ :as pos] position
                 n gen/pos-int]
    (-> (day1/forward n)
        (cm/mmul pos)
        (as-> x (take 2 x))
        (cm/distance [x y])
        (== n))))

;; The orientation before and after turning left
;; should have a dot product of zero.
(defspec turn-left-orthogonal
  (prop/for-all [[_ _ dx dy :as pos] position]
    (-> day1/turn-left
        (cm/mmul pos)
        (as-> x (drop 2 x))
        (cm/inner-product [dx dy])
        (zero?))))

(defspec turn-right-orthogonal
  (prop/for-all [[_ _ dx dy :as pos] position]
    (-> day1/turn-right
        (cm/mmul pos)
        (as-> x (drop 2 x))
        (cm/inner-product [dx dy])
        (zero?))))

;; Turns don't affect coordinates.
(defspec turn-left-coordinate-identity
  (prop/for-all [[x y _ _ :as pos] position]
    (-> day1/turn-left
        (cm/mmul pos)
        (as-> x (take 2 x))
        (cm/e== [x y]))))

(defspec turn-right-coordinate-identity
  (prop/for-all [[x y _ _ :as pos] position]
    (-> day1/turn-right
        (cm/mmul pos)
        (as-> x (take 2 x))
        (cm/e== [x y]))))

;; Properties test using constrained directions.

(defn concatenating
  [generator]
  (gen/fmap (partial into [] cat) generator))

(def right-left
  "Generates a right, then left turn, each walking n steps."
  (-> gen/s-pos-int
      (gen/bind (fn [n] (gen/tuple (gen/return [:R n])
                                   (gen/return [:L n]))))))

(def clockwise
  "Generates directions in a clockwise square path.
  Effectively maintains position and direction."
  (-> gen/s-pos-int
      (gen/bind (fn [n] (gen/tuple (gen/return [:R n])
                                   (gen/return [:R n])
                                   (gen/return [:R n])
                                   (gen/return [:R n]))))))

(def counter-clockwise
  "Generates directions in a counter-clockwise square path.
  Effectively maintains position and direction."
  (-> gen/s-pos-int
      (gen/bind (fn [n] (gen/tuple (gen/return [:L n])
                                   (gen/return [:L n])
                                   (gen/return [:L n])
                                   (gen/return [:L n]))))))

(def uturn
  "Generates directions for a uturn.
  Effectively maitains position, but reverses direction."
  (-> gen/s-pos-int
      (gen/bind (fn [n] (gen/tuple (gen/return [:R n])
                                   (gen/return [:L n])
                                   (gen/return [:L n])
                                   (gen/return [:L n]))))))

;; When traveling diagonally, the x and y position of the destination
;; (after offsetting initial position) should have equal magnitude.
(defspec diagonal-travel
  (prop/for-all [[ix iy _ _ :as init] position
                 dirs (concatenating (gen/vector right-left))]
    (let [[x y _ _] (day1/int-coords (day1/travel init dirs))]
      (= (util/abs (- x ix))
         (util/abs (- y iy))))))

;; Even with loops.
(defspec diagonal-travel-with-loops
  (prop/for-all [[ix iy _ _ :as init] position
                 dirs (concatenating (gen/vector
                                      (gen/one-of [right-left
                                                   clockwise
                                                   counter-clockwise])))]
    (let [[x y _ _] (day1/int-coords (day1/travel init dirs))]
      (= (util/abs (- x ix))
         (util/abs (- y iy))))))

;; First intersection (if any) should be the first uturn,
;; which should be on the diagonal path.
(defspec first-loop-intersection
  (prop/for-all [[ix iy _ _ :as init] position
                 dirs (concatenating (gen/vector
                                      (gen/one-of [right-left
                                                   uturn])))]
    (let [[x y :as intersect] (day1/first-intersect init dirs)]
      (or (nil? intersect) (= (util/abs (- x ix))
                              (util/abs (- y iy)))))))

