(ns d9
  (:require [clojure.java.io :as io]))

;The first step of attacking the weakness in the XMAS data is to find the first number in the list
;(after the preamble) which is not the sum of two of the 25 numbers before it.
;What is the first number that does not have this property?

(defn read-cryptogram [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         (mapv #(Long/parseLong %)))))

(defn check-last-number [numbers]
  (let [n (last numbers)
        factors (set (take 25 numbers))]
    (->> factors
         (map #(- n %))
         (some #(contains? factors %)))))

(defn answer []
  (let [crypto (read-cryptogram "resources/d9.txt")]
    (->> crypto
         (partition 26 1)
         (filter #(not (check-last-number %)))
         first
         last)))

;The final step in breaking the XMAS encryption relies on the invalid number you just found:
;you must find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
;To find the encryption weakness, add together the smallest and largest number in this contiguous range

(defn check-number [weakness-nr numbers]
  (loop [[n & rest] numbers
         seq []
         acc 0]
    (if (= 0 (- weakness-nr acc))
      seq
      (if (> acc weakness-nr)
        []
        (recur rest (conj seq n) (+ acc n))))))

(defn answer2 []
  (let [crypto (read-cryptogram "resources/d9.txt")
        weakness-nr 776203571]
    (->> crypto
         count
         range
         (map #(check-number weakness-nr (drop % crypto)))
         (filter not-empty)
         first
         (#(list (apply min %) (apply max %)))
         (apply +))))

(defn find-seq [nr numbers]
  (loop [i 0
         k 0
         s (- nr)]
    (cond
      (neg? s) (recur i (inc k) (+ s (numbers k)))
      (pos? s) (recur (inc i) k (- s (numbers i)))
      :otherwise
      (subvec numbers i k))))

(defn better-answer2 []
  (let [crypto (read-cryptogram "resources/d9.txt")
        weakness-nr 776203571]
    (->> crypto
         (find-seq weakness-nr)
         sort
         (#(+ (first %) (last %))))))
