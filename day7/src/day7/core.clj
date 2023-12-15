(ns day7.core
  (:require
    [clojure.string :as string]))

(defn is-five-of-a-kind
  "There is one type of card"
  [hand]
  (= 1 (count (frequencies hand))))

(defn is-four-of-a-kind
  "There exists a frequency with a count of 4"
  [hand]
  (true? (some (fn [[_ v]] (= 4 v)) (frequencies hand))))

(defn is-full-house
  "There are two types of card and one of them has a count of 3"
  [hand]
  (let [freqs (frequencies hand)]
    (true? (and (= 2 (count freqs))
                (some (fn [[_ v]] (= 3 v)) freqs)))))

(defn is-three-of-a-kind
  "There are more than two types of card and one of them has a count of 3"
  [hand]
  (let [freqs (frequencies hand)]
    (true? (and (> (count freqs) 2)
                (some (fn [[_ v]] (= 3 v)) freqs)))))

(defn is-two-pair
  "There are two types of card with a count of 2"
  [hand]
  (= 2 (count (filter #(= 2 (% 1)) (frequencies hand)))))

(defn is-one-pair
  "There are four types of card"
  [hand]
  (= 4 (count (frequencies hand))))

(defn is-high-card
  "There are five types of card"
  [hand]
  (= 5 (count (frequencies hand))))

(defn extract-hand
  "Pull the hand and the bid out of the input line"
  [s]
  (let [[hand bid] (map first (re-seq #"([TJQKA\d]+)" s))]
    [hand (read-string bid)]))

(defn map-hand-to-rank
  "Convert a hand to the type it is"
  [hand]
  (cond
    (is-five-of-a-kind hand) 6
    (is-four-of-a-kind hand) 5
    (is-full-house hand) 4
    (is-three-of-a-kind hand) 3
    (is-two-pair hand) 2
    (is-one-pair hand) 1
    (is-high-card hand) 0))

(defn sort-hands
  "Try to sort by hand type; if they are the same, compare each card in turn"
  [[hand-a] [hand-b] map-fn ranks]
  (let [rank-a (map-fn hand-a)
        rank-b (map-fn hand-b)
        rank-comp (compare rank-a rank-b)]
    (if (not= 0 rank-comp)
      rank-comp
      (loop [x 0]
        (if (> x 4)
          0
          (let [card-a (str (get hand-a x))
                card-b (str (get hand-b x))
                card-comp (compare (.indexOf ranks card-a) (.indexOf ranks card-b))]
            (if (= 0 card-comp) (recur (+ x 1)) card-comp)))))))

(def ranks ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"])
(def part-2-ranks ["J" "2" "3" "4" "5" "6" "7" "8" "9" "T" "Q" "K" "A"])

(defn -main
  "AoC day 7"
  [map-fn ranks]
  (let [hands (map extract-hand (string/split-lines (slurp "./input.txt")))]
    (->> (sort #(sort-hands %1 %2 map-fn ranks) hands)
         (map last)
         (map-indexed #(* (+ 1 %1) %2))
         (reduce +))))

(defn try-all-joker-combinations
  "Get the best hand by replacing jokers by all possible other cards"
  [hand]
  (apply max (for [rank ranks] (map-hand-to-rank (string/replace hand "J" rank)))))

(defn part-1
  []
  (-main map-hand-to-rank ranks))

(defn part-2
  []
  (-main try-all-joker-combinations part-2-ranks))