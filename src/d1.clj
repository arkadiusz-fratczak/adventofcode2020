(ns d1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;--- Day 1: Report Repair ---
;After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
;The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
;To save your vacation, you need to get all fifty stars by December 25th.
;Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
;Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
;Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
;For example, suppose your expense report contained the following:
;
;1721
;979
;366
;299
;675
;1456
;
;In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
;
;Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

(defn read-nrs [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [i] (Integer/parseInt i)))))

(defn answer1 []
  (let [nrs (read-nrs "resources/d1_input")]
    (for [x nrs
          y nrs
          :when (= 2020 (+ x y))
          :when (< x y)
          :let [xy (* x y)]]
      {:x x :y y :xy xy})))

(defn answer2 []
  (let [nrs (read-nrs "resources/d1_input")]
    (for [x nrs
          y nrs
          z nrs
          :when (= 2020 (+ x y z))
          :when (< x y z)
          :let [xyz (* x y z)]]
      {:x x :y y :z z :xyz xyz})))

(defn answer-impr []
  (let [nrs (with-open [rdr (io/reader "resources/d1_input")]
              (->> rdr
                   line-seq
                   (map (fn [i] (Integer/parseInt i)))
                   set))]
    (for [x nrs
          :let [y (- 2020 x)]
          :when (< x y)
          :when (contains? nrs y)]
      {:x x :y y :xy (* x y)})))