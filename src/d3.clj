(ns d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
;The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
;From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
;The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:
;..##.........##.........##.........##.........##.........##.......  --->
;#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
;.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
;..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
;.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
;..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
;.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
;.#........#.#........X.#........#.#........#.#........#.#........#
;#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
;#...##....##...##....##...#X....##...##....##...##....##...##....#
;.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
;In this example, traversing the map using this slope would cause you to encounter 7 trees.

(defn parse-line [line]
  (mapv (fn [ch] (if (= "." ch) 0 1)) (str/split line #"")))

(defn read-map [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         (mapv parse-line))))

(defn answer
  ([] (answer 0 3 1 3 1 (read-map "resources/d3_input.txt")))
  ([right down] (answer 0 right down right down (read-map "resources/d3_input.txt")))
  ([tacc racc dacc right down matrix]
   (if (>= dacc (count matrix))
     tacc
     (let [row (nth matrix dacc)
           val (nth row (mod racc (count row)))
           trees (+ tacc val)]
       (answer trees (+ racc right) (+ dacc down) right down matrix)))))

;--- Part Two ---
;Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.
;Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
;Right 1, down 1.
;Right 3, down 1. (This is the slope you already checked.)
;Right 5, down 1.
;Right 7, down 1.
;Right 1, down 2.
;In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.

(defn answer2 []
  (let [slopes [{:r 1 :d 1} {:r 3 :d 1} {:r 5 :d 1} {:r 7 :d 1} {:r 1 :d 2}]]
    (reduce * (map #(answer (:r %) (:d %)) slopes))))