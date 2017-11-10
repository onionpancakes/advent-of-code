(ns advent.yr2016.day4-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.java.io :refer [resource]]
            [clojure.string :as c.str]
            [clojure.set :as c.set]
            [advent.yr2016.day4 :as day4]))

;; Answers according to advent website.

(deftest answers
  (let [input (slurp (resource "yr2016/day4/input.txt"))]
    (is (= "137896" (day4/answer1 input)))
    (is (= "501" (day4/answer2 input)))))

;; Unit tests

(deftest parse-room-test
  (are [r s] (= r (day4/parse-room s))
    {::day4/room "a-1[a]"
     ::day4/encrypted "a"
     ::day4/sector "1"
     ::day4/checksum "a"} "a-1[a]"
    {::day4/room "abc-123[abc]"
     ::day4/encrypted "abc"
     ::day4/sector "123"
     ::day4/checksum "abc"} "abc-123[abc]"
    {::day4/room "abc-edf-hij-123[abc]"
     ::day4/encrypted "abc-edf-hij"
     ::day4/sector "123"
     ::day4/checksum "abc"} "abc-edf-hij-123[abc]"
    {::day4/room "----------123[a]"
     ::day4/encrypted "---------"
     ::day4/sector "123"
     ::day4/checksum "a"} "----------123[a]"))

(deftest sort-common-alpha-test
  (are [r s] (= r (day4/sort-common-alpha s))
    [] ""
    [\a] "a"
    [\a \b \c] "abc"
    [\a \b \c] "cba"
    [\a \a \b \c] "aabc"
    [\c \c \a \b] "abcc"
    [\b \b \b \c \c \a] "ccabbb"))

(deftest matching-checksum?-test
  (testing "valid checksums"
    (are [e c] (day4/matching-checksum? e c)
      "" ""
      "a" "a"
      "aa" "a"
      "aab" "a"
      "aabc" "ab"
      "caba" "ab"))
  (testing "invalid checksums"
    (are [e c] (not (day4/matching-checksum? e c))
      "" "a"
      "ab" "b"
      "abb" "a"
      "abbccc" "b")))

(deftest real?-test
  (testing "real room maps"
    (are [m] (day4/real? m)
      {::day4/encrypted "abcde"
       ::day4/checksum "abcde"}
      {::day4/encrypted "abccdeff"
       ::day4/checksum "abcdf"})))

(deftest relative-shift-test
  (are [r start window shift] (= r (day4/relative-shift start window shift))
    0 0 1 0
    0 0 1 5
    0 0 1 -5
    1 5 10 1
    1 5 10 11
    -1 5 10 -1
    -1 5 10 -11))

(deftest shift-lower-alpha-test
  (are [r ch shift] (= r (day4/shift-lower-alpha ch shift))
    \a \a 0
    \e \e 0
    \b \a 1
    \b \a 27
    \a \b -1
    \a \b -27))

(deftest shift-lower-alpha-string-test
  (are [r s shift] (= r (day4/shift-lower-alpha-string s shift))
    "abc" "abc" 0
    "bcd" "abc" 1
    "bcd" "abc" 27
    "abc" "bcd" -1
    "abc" "bcd" -27))

(deftest decrypt-test
  (are [decrypted rm] (let [drm (day4/decrypt rm)]
                        (and (contains? drm ::day4/decrypted)
                             (= decrypted (::day4/decrypted drm))))
    "abc" {::day4/encrypted "abc"
           ::day4/sector "0"}
    "abc" {::day4/encrypted "abc"
           ::day4/sector "26"}
    "bcd" {::day4/encrypted "abc"
           ::day4/sector "1"}
    "bcd" {::day4/encrypted "abc"
           ::day4/sector "27"}))

;; Properties tests

(def lower-alpha
  (gen/fmap char (gen/choose (int \a) (int \z))))

(def lower-alpha-string
  (gen/fmap c.str/join (gen/vector lower-alpha)))

(def hyphen
  (gen/return \-))

(def encrypted
  (gen/fmap c.str/join (gen/vector (gen/one-of [lower-alpha hyphen]))))

(def sector
  (gen/fmap (partial apply str)
            (gen/not-empty (gen/vector (gen/choose 0 9)))))

(def checksum
  lower-alpha-string)

(def room-string
  (gen/fmap (fn [[e s c]] (str e "-" s "[" c "]"))
            (gen/tuple encrypted sector checksum)))

(def room-map
  (gen/fmap day4/parse-room room-string))

;; Shifting, then shifting again in the opposite direction
;; should produce the same string.
(defspec shift-unshift
  (prop/for-all [shift gen/int
                 s lower-alpha-string]
    (= s (-> s
             (day4/shift-lower-alpha-string shift)
             (day4/shift-lower-alpha-string (- shift))))))

;; Encrypted and decrypted strings should have the
;; same numerical distribution of characters.
(defspec encrypted-decrypted-distribution
  (prop/for-all [r room-map]
    (let [{encrypted ::day4/encrypted
           decrypted ::day4/decrypted} (day4/decrypt r)]
      (= (-> (frequencies encrypted)
             (dissoc \-)
             (vals)
             (sort))
         (-> (frequencies decrypted)
             (dissoc \space)
             (vals)
             (sort))))))

