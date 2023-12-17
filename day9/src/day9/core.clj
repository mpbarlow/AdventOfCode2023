(ns day9.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn extract-sequence
  "Extract a string sequence into a vector"
  [s]
  (into [] (map (comp read-string last) (re-seq #"(-?\d+)" s))))

(defn calc-next-sequence
  "Calculate the next sequence by subtracting the previous value from each value"
  [sequence]
  (into [] (for [i (range 1 (count sequence))] (- (sequence i) (sequence (- i 1))))))

(defn process-sequence
  [sequence]
  (if (every? zero? sequence)
    0
    (+ (last sequence) (process-sequence (calc-next-sequence sequence)))))

(defn process-sequence-reverse
  [sequence]
  (if (every? zero? sequence)
    0
    (- (first sequence) (process-sequence-reverse (calc-next-sequence sequence)))))

(defn -main
  "AoC day9"
  [& args]
  (let [sequences (map extract-sequence (string/split-lines (slurp "./input.txt")))]
    (reduce + (map process-sequence sequences))))

(defn part-two
  []
  (let [sequences (map extract-sequence (string/split-lines (slurp "./input.txt")))]
    (reduce + (map process-sequence-reverse sequences))))
