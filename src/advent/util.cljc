(ns advent.util)

(defn parse-int
  [istr]
  #?(:clj (Integer/parseInt istr)))

(defn abs
  [n]
  #?(:clj (Math/abs n)))

