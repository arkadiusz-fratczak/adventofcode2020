(ns d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;Immediately before the program would run an instruction a second time, the value in the accumulator is 5.
;Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?

(defn read-program [filename]
  (with-open [rdr (io/reader filename)]
    (->> rdr
         line-seq
         (map #(str/split % #" "))
         (map (fn [[op n]] (list (keyword op) (Integer/parseInt n))))
         vec)))

(defn run-program [code pos acc visited-lines]
  (cond
    (>= pos (count code)) {:status "ok"
                           :acc acc}
    (contains? visited-lines pos) {:status "nok"
                                   :acc acc}
    :else (let [cmd (first (nth code pos))
                n (second (nth code pos))
                vl (conj visited-lines pos)]
            (case cmd
              :acc (recur code (inc pos) (+ acc n) vl)
              :nop (recur code (inc pos) acc vl)
              :jmp (recur code (+ pos n) acc vl)))))

(defn answer []
  (let [code (read-program "resources/d8.txt")]
    (run-program code 0 0 #{})))

;Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
;What is the value of the accumulator after the program terminates?

(defn fix-code [code line]
  (case (first (nth code line))
    :acc code
    :nop (update-in code [line] (fn [[_ n]] (list :jmp n)))
    :jmp (update-in code [line] (fn [[_ n]] (list :nop n)))))

(defn answer2 []
  (let [code (read-program "resources/d8.txt")]
    (for [line (range (count code))
          :let [c (fix-code code line)
                r (run-program c 0 0 #{})]
          :when (= "ok" (:status r))]
      r)))