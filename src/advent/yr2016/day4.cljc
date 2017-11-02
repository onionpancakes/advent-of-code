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
  (->> (frequencies chs)
       ;; the (comp - int key) impls alphabetical sort.
       (sort-by (juxt val (comp - int key))
                #(compare %2 %1))
       (map key)))

(defn valid-checksum?
  "True if checksum contains the most common
  characters in encrypted, with ties broken in
  alphabetical order."
  [checksum encrypted]
  (let [chset (set checksum)]
    (->> encrypted
         (eduction (remove (partial = \-)))
         (sort-common-alpha)
         (into #{} (take (count chset)))
         (= chset))))

(defn real?
  "Given a parsed room map, return true if it is real.
  Rooms are considered real if the checksum has 5
  unique characters and checksum is valid with its
  encrypted string."
  [{:keys [::encrypted ::checksum]}]
  (and (= (count (set checksum)) 5)
       (valid-checksum? checksum encrypted)))

(defn answer1
  [input]
  (->> (cs/split (cs/trim input) #"\n")
       (eduction (comp (map parse-room)
                       (filter real?)
                       (map ::sector)
                       (map util/parse-int)))
       (reduce + 0)))

;; Part 2

(defn shift-lower-alpha
  "Shift a lowercase character."
  [ch n-shift]
  (-> (int ch)
      (- (int \a))
      (+ n-shift)
      (mod 26)
      (+ (int \a))
      (char)))

(def lower-alpha
  (into [] (map char) (range (int \a) (inc (int \z)))))

(defn shift
  "Shifts lowercase characters in a string."
  [text n-shift]
  (let [chmapping (->> lower-alpha
                       (map #(shift-lower-alpha % n-shift))
                       (zipmap lower-alpha))]
    (->> text
         (map #(get chmapping % %))
         (apply str))))

(defn decrypt
  "Decrypt a room map."
  [{:keys [::encrypted ::sector] :as room}]
  (assoc room ::decrypted
         (-> encrypted
             (shift (util/parse-int sector))
             (cs/replace #"-" " "))))

(defn answer2
  [input]
  (->> (cs/split (cs/trim input) #"\n")
       (into [] (comp (map parse-room)
                      (filter real?)
                      (map decrypt)
                      (filter (comp #(re-find #"north" %)
                                    ::decrypted))
                      (map ::sector)))
       (first)))

