(ns d2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


;--- Day 2: Password Philosophy ---
;Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.
;The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.
;Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.
;To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.
;For example, suppose you have the following list:
;
;1-3 a: abcde
;1-3 b: cdefg
;2-9 c: ccccccccc
;
;Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
;In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
;How many passwords are valid according to their policies?


(defn valid-password? [pwd-line]
  (let [str-columns (str/split pwd-line #" ")
        min-max (str/split (nth str-columns 0) #"-")
        pwd (nth str-columns 2)
        letter (first (nth str-columns 1))
        min (Integer/parseInt (first min-max))
        max (Integer/parseInt (second min-max))
        letter-count (get (frequencies pwd) letter 0)]
    (and (<= letter-count max) (>= letter-count min))))

(defn answer []
  (->> "resources/d2_input.txt"
       slurp
       str/split-lines
       (filter valid-password?)
       count))

(defn parse-line [line]
  (let [[_ min max ch pwd] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :ch (nth ch 0)
     :pwd pwd}))

(defn read-database [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         (mapv parse-line))))

(defn impr-valid-password? [{:keys [min max ch pwd]}]
  (let [times (->> pwd
                   (filter #(= ch %))
                   count)]
    (<= min times max)))

(defn impr-answer []
  (->> "resources/d2_input.txt"
       read-database
       (filter impr-valid-password?)
       count))

;--- Part Two ---
;While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.
;The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.
;Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
;
;Given the same example list from above:
;1-3 a: abcde is valid: position 1 contains a and position 3 does not.
;1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
;2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
;How many passwords are valid according to the new interpretation of the policies?

(defn new-valid-password? [pwd-line]
  (let [str-columns (str/split pwd-line #" ")
        idxs (str/split (nth str-columns 0) #"-")
        pwd (nth str-columns 2)
        letter (first (nth str-columns 1))
        idx1 (Integer/parseInt (first idxs))
        idx2 (Integer/parseInt (second idxs))
        letter-at-idx1 (nth pwd (- idx1 1))
        letter-at-idx2 (nth pwd (- idx2 1))]
    (and
     (not= letter-at-idx1 letter-at-idx2)
     (or (= letter-at-idx1 letter)
         (= letter-at-idx2 letter)))))

(defn answer2 []
  (->> "resources/d2_input.txt"
       slurp
       str/split-lines
       (filter new-valid-password?)
       count))
