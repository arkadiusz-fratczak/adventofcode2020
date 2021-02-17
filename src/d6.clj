(ns d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;--- Day 6: Custom Customs ---
;As your flight approaches the regional airport where you'll switch to a much larger plane, customs declaration forms are distributed to the passengers.
;The form asks a series of 26 yes-or-no questions marked a through z. All you need to do is identify the questions for which anyone in your group answers "yes". Since your group is just you, this doesn't take very long.
;However, the person sitting next to you seems to be experiencing a language barrier and asks if you can help. For each of the people in their group, you write down the questions for which they answer "yes", one per line. For example:
;abcx
;abcy
;abcz
;In this group, there are 6 questions to which anyone answered "yes": a, b, c, x, y, and z. (Duplicate answers to the same question don't count extra; each question counts at most once.)
;For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?

(defn split [pattern data]
  (str/split data pattern))

(defn read-grouped-declarations [filename]
  (->> filename
      slurp
      (split #"\n\n")))

(defn answer []
  (->> "resources/d6.txt"
       read-grouped-declarations
       (map #(str/replace % #"\n" ""))
       (map #(count (set %)))
       (reduce +)))

;--- Part Two ---
;As you finish the last group's customs declaration, you notice that you misread one word in the instructions:
;You don't need to identify the questions to which anyone answered "yes"; you need to identify the questions to which everyone answered "yes"!

(defn answer2 []
  (->> "resources/d6.txt"
       read-grouped-declarations
       (map #(apply set/intersection (map set (split #"\n" %))))
       (map count)
       (reduce +)))