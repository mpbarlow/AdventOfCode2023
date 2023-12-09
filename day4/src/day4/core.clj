(ns day4.core
  (:gen-class)
  (:require
    [clojure.set]
    [clojure.string]
    [clojure.math]))

(defn get-numbers
  [s]
  (->> (re-seq #"(\d+)" s)
       (map first)
       (map read-string)))

(defn extract-numbers-matching
  [r s]
  (->> (re-find r s)
       last
       get-numbers
       set))

(defn extract-winning-numbers
  [s]
  (extract-numbers-matching #"Card\s+\d+:\s+((?:\d+\s*)+)\|" s))

(defn extract-card-numbers
  [s]
  (extract-numbers-matching #"\|\s+((?:\d+\s*)+)" s))

(defn get-matches
  []
  (->> (slurp "./input.txt")
       clojure.string/split-lines
       (map #(clojure.set/intersection (extract-card-numbers %) (extract-winning-numbers %)))
       (into [])))

(defn add-copies
  "Add num-to-add copies of the next num-cards cards"
  [copies num-cards num-to-add]
  (into []
        (for [index (range 0 (max num-cards (count copies)))]
          (let [current-copy-count (or (get copies index) 1)]
            (if (>= index num-cards)
              current-copy-count
              (+ current-copy-count num-to-add))))))

(defn part-two
  []
  (loop [cards 0
         to-process (get-matches)
         upcoming-copies []]
    (if (empty? to-process)
      cards
      (let [copies-of-current (or (first upcoming-copies) 1)
            matches (count (first to-process))]
        ; Add the number of copies of the current card to the total card count, and add copies of the next
        ; cards based on how many matches there are
        (recur (+ cards copies-of-current)
               (subvec to-process 1)
               (add-copies (into [] (drop 1 upcoming-copies))
                           matches
                           copies-of-current))))))

(defn -main
  "AoC day 4"
  [& args]
  (->> (get-matches)
       (filter #(> (count %) 0))
       (map #(clojure.math/pow 2 (- (count %) 1)))
       (reduce +)))
