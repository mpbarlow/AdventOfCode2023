(ns day6.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn get-numbers
  [s]
  (mapv (comp read-string first) (re-seq #"(\d+)" s)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [lines (string/split-lines (slurp "./input.txt"))
        races (map vector (get-numbers (lines 0)) (get-numbers (lines 1)))
        winning-combos (for [[time record] races]
                         (->> (for [hold-time (range 1 time)] (* hold-time (- time hold-time)))
                              (filter #(> % record))
                              count))]
    (reduce * winning-combos)))

(defn glue-numbers
  [s]
  (read-string (apply str (map first (re-seq #"(\d+)" s)))))

(defn part-two
  []
  (let [lines (string/split-lines (slurp "./input.txt"))
        time (glue-numbers (lines 0))
        record (glue-numbers (lines 1))]
    (count (filter #(> (* % (- time %)) record) (range 1 time)))))
