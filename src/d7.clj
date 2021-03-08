(ns d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as w]
            [clojure.set :as set]))

;--- Day 7: Handy Haversacks ---
;You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.
;Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
;For example, consider the following rules:
;light red bags contain 1 bright white bag, 2 muted yellow bags.
;dark orange bags contain 3 bright white bags, 4 muted yellow bags.
;bright white bags contain 1 shiny gold bag.
;muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
;shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
;dark olive bags contain 3 faded blue bags, 4 dotted black bags.
;vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
;faded blue bags contain no other bags.
;dotted black bags contain no other bags.
;These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
;You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
;In the above rules, the following options would be available to you:
;A bright white bag, which can hold your shiny gold bag directly.
;A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
;A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
;A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
;So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.
;How many bag colors can eventually contain at least one shiny gold bag?

(defn parse-content [seq]
  (into {}
        (->> seq
             (map #(str/split % #"\s"))
             (map (fn [[k v]] [(keyword v) (Integer/parseInt k)])))))

(defn parse-bag-rule [serialized]
  (->> serialized
       (re-seq #"^\w+\s\w+|\d\s\w+\s\w+")
       (map #(str/replace % #"(\p{L})\s(\p{L})" "$1-$2"))
       ((fn [s] {(keyword (first s))
                 (parse-content (rest s))}))))

(defn read-bag-rules [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         vec
         (map parse-bag-rule))))

(defn search-containers [bags bag-color]
  (->> bags
       (filter #(contains? (val (first %)) bag-color))
       (map #(key (first %)))
       set))

(defn search-all-containers
  ([bags bag-color]
   (search-all-containers bags bag-color #{}))
  ([bags bag-color acc]
   (let [direct-cont (search-containers bags bag-color)]
     (if (empty? direct-cont)
       (conj acc bag-color)
       (conj (reduce #(set/union %1 (search-all-containers bags %2 #{}))
                     acc
                     direct-cont)
             bag-color)))))

(defn answer []                                             ; 229
  (let [bag-rules (read-bag-rules "resources/d7.txt")]
    (->> (search-all-containers bag-rules :shiny-gold)
         count
         dec)))

(defn sub-contains? [bags container own-color]
  (->> container
       bags
       (some
        (fn [[n color]]
          (or
           (= color own-color)
           (sub-contains?  bags color own-color))))))

;--- Part Two ---
;It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!
;Consider again your shiny gold bag and the rules from the above example:
;faded blue bags contain 0 other bags.
;dotted black bags contain 0 other bags.
;vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
;dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
;So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!
;Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!
;How many individual bags are required inside your single shiny gold bag?

(defn count-bags
  [bag-color bags]
  (let [comps (get bags bag-color)]
    (if (empty? comps)
      0
      (reduce #(+ %1 (val %2) (* (val %2) (count-bags (key %2) bags)))
              0
              comps))))

(defn answer2 []                                            ; 6683
  (->> (read-bag-rules "resources/d7.txt")
       (apply merge)
       (count-bags :shiny-gold)))