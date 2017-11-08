(ns advent.yr2016.day4
  (:require [clojure.string :as cs]
            [advent.util :as util]))

(def room-pattern
  #"([a-z\-]+)-(\d+)\[([a-z]+)\]")

(defn parse-room
  [room-string]
  (->> (re-find room-pattern room-string)
       (zipmap [::room ::encrypted ::sector ::checksum])))

(defn sort-common-alpha
  "Sorts a sequence of chars in order of descending
  frequency, with ties broken in alphabetical order."
  [chs]
  (sort-by (juxt (frequencies chs) (comp - int))
           #(compare %2 %1)
           chs))

(defn matching-checksum?
  "True if checksum contains the most common
  characters in encrypted, with ties broken in
  alphabetical order."
  [encrypted checksum]
  (let [chset (set checksum)]
    (->> encrypted
         (eduction (remove (partial = \-)))
         (sort-common-alpha)
         (eduction (dedupe))
         (into #{} (take (count chset)))
         (= chset))))

(defn real?
  "Given a parsed room map, return true if it is real.
  Rooms are considered real if the checksum has 5
  unique characters and checksum is valid with its
  encrypted string."
  [{:keys [::encrypted ::checksum]}]
  (and (= (count (set checksum)) 5)
       (matching-checksum? encrypted checksum)))

(defn answer1
  [input]
  (->> (cs/split input #"\n")
       (eduction (comp (map cs/trim)
                       (map parse-room)))
       (eduction (comp (filter real?)
                       (map ::sector)
                       (map util/parse-int)))
       (reduce + 0)
       (str)))

;; Part 2

(defn relative-shift
  "Given a shift, and a start index between 0 and
  window-size, returns the relative shift from the
  start index."
  [start window-size shift]
  (- (mod (+ start shift) window-size) start))

(defn shift-lower-alpha
  "Shift a lowercase character."
  [ch shift]
  (-> (- (int ch) (int \a))
      (relative-shift 26 shift)
      (+ (int ch))
      (char)))

(def lower-alpha
  "Lowercase chars from a to z."
  (into [] (map char) (range (int \a) (inc (int \z)))))

(defn shift-lower-alpha-string
  "Shifts lowercase characters in a string."
  [s shift]
  (let [shift-entry (juxt identity #(shift-lower-alpha % shift))
        chmapping (into {} (map shift-entry) lower-alpha)]
    (->> (map #(get chmapping % %) s)
         (apply str))))

(defn decrypt
  "Decrypt a room map."
  [{:keys [::encrypted ::sector] :as room}]
  (-> encrypted
      (shift-lower-alpha-string (util/parse-int sector))
      (cs/replace #"-" " ")
      (as-> x (assoc room ::decrypted x))))

(defn answer2
  [input]
  (->> (cs/split input #"\n")
       (eduction (comp (map cs/trim)
                       (map parse-room)))
       (eduction (comp (filter real?)
                       (map decrypt)
                       (filter (comp #(re-find #"north" %)
                                     ::decrypted))
                       (map ::sector)))
       (first)))

