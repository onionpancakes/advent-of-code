(ns dev.onionpancakes.adventofcode.2016.util
  (:refer-clojure :exclude [abs]))

(defn parse-int
  [istr]
  #?(:clj (try
            (Integer/parseInt istr)
            (catch NumberFormatException _
              (bigint istr)))))

(defn abs
  [n]
  #?(:clj (Math/abs n)))

