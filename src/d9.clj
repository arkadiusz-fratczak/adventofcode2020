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

(defn answer []
  (let [crypto (read-cryptogram "resources/d9.txt")]))
