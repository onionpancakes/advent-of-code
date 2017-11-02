(ns advent.yr2016.day1
  (:require [clojure.core.matrix :as cm]
            [clojure.string :as cs]
            [advent.util :as util]))

;; Strategy: compute the answer using translation
;; and rotation matrixes.

;; Vectors have 4 dimensions: [x y dx dy]
;; x and y are coordinates
;; dx and dy are directions for x and y.
;; East is when dx = 1; west when dx = -1.
;; North is when dy = 1; south when dy = -1.

;; e.g. Vector [2 3 0 1]
;; At coordinate [2 3], facing north.

(defn parse-direction
  "Parse a direction string into a keyword-int vector.
  e.g. R4 -> [:R 4]"
  [dstr]
  [(-> (subs dstr 0 1)
       (keyword))
   (-> (subs dstr 1 (count dstr))
       (util/parse-int))])

(defn forward
  "Matrix which translates position forward by n steps."
  [n]
  [[1 0 n 0]
   [0 1 0 n]
   [0 0 1 0]
   [0 0 0 1]])

(def turn-left
  "Matrix which rotates direction left."
  [[1 0 0 0]
   [0 1 0 0]
   [0 0 0 -1]
   [0 0 1 0]])

(def turn-right
  "Matrix which rotates direction right."
  [[1 0 0 0]
   [0 1 0 0]
   [0 0 0 1]
   [0 0 -1 0]])

(def turn-mapping
  {:L turn-left
   :R turn-right})

(defn direction-matrix
  "Convert direction vector into its matrix equivalent."
  [[turn paces]]
  (cm/mmul (forward paces) (turn-mapping turn)))

(defn answer1
  [input]
  (->> (cs/split input #",")
       (eduction (comp (map cs/trim)
                       (map parse-direction)))
       (transduce (map direction-matrix)
                  (completing #(cm/mmul %2 %1))
                  [0 0 0 1]) ; at origin, facing north
       ;; Sum the abs of the coordinates.
       (take 2)
       (map util/abs)
       (apply +)
       (int)))


;; Part 2

(defn steps
  "Given a vertical or horizontal line, return all
  steps between the line's endpoints (inclusive)."
  [[[xa ya] [xb yb]]]
  (cond
    (= xa xb) (->> (if (<= ya yb)
                     (range ya (inc yb))
                     (range ya (dec yb) -1))
                   (map #(vector xa %)))
    (= ya yb) (->> (if (<= xa xb)
                     (range xa (inc xb))
                     (range xa (dec xb) -1))
                   (map #(vector % ya)))))

(defn first-duplicate
  [xs]
  (loop [seen #{} items (seq xs)]
    (if items
      (let [item (first items)]
        (if (contains? seen item)
          item
          (recur (conj seen item) (next items)))))))

(defn answer2
  [input]
  (->> (cs/split input #",")
       (map (comp direction-matrix
                  parse-direction
                  cs/trim))
       ;; Get all vectors for each direction step.
       (reductions #(cm/mmul %2 %1) [0 0 0 1])
       ;; Get coordinates from vectors, cast to ints.
       (map (comp #(into [] (map int) %)
                  #(subvec % 0 2)))
       ;; Partition coordinates as line segments.
       (partition 2 1)
       ;; Line segments into inbetween steps.
       (map steps)
       ;; Remove overlapping endpoints.
       (map-indexed #(if-not (zero? %1)
                       (rest %2)
                       %2))
       (mapcat identity)
       (first-duplicate)

       (map util/abs)
       (apply +)
       (int)))
