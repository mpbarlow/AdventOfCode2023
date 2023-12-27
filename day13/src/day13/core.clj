(ns day13.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn parse-patterns [s]
  ; -1 keeps the blank lines so we can delimit patterns
  (->> (string/split s #"\n" -1)
       (reduce (fn [carry line] (if (= "" line)
                                  (conj carry [])
                                  (update carry (dec (count carry)) conj line)))
               [[]])
       (mapv (fn [v] (mapv #(string/split % #"") v)))))

(defn rotate-clockwise
  "Based off transposition from https://stackoverflow.com/a/10347404
  map/mapv accepts multiple collections, so we end up taking a vector of the first element from each collection, then
  the second, etc -- i.e. a transposition.
  However based on our rules of symmetry -- columns to the left for horizontal and rows above for vertical, we the first
  row to become the _last_ column, etc. So we reverse each resulting line."
  [m]
  (mapv #(apply vector (rseq %)) (apply mapv vector m)))

(defn is-symmetrical-to-edge?
  "Given a starting pair of symmetrical line indexes (line and mirrored), would outwards to verify the pattern remains
  (vertically) symmetrical until it goes off the edge."
  [patt line mirrored]
  (loop [line line
         mirrored mirrored]
    (cond
      (< line 0) true
      (>= mirrored (count patt)) true
      (not= (patt line) (patt mirrored)) false
      :else (recur (dec line) (inc mirrored)))))

(defn find-vert-symmetry
  "With a rolling window of three lines, identify a potential starting point of symmetry, then run a check to see if
  the pattern is symmetrical along this (rather than it just being two lines that are the same)."
  [patt ignoring]
  (loop [start 0]
    (let [end (+ start 1)]
      (cond
        (>= end (count patt)) 0
        ; Try between the second and third rows
        (and (= (patt start) (patt end))
             (is-symmetrical-to-edge? patt start end)
             (or (nil? ignoring) (not= ignoring end))) end
        :else (recur (inc start))))))

(defn -main
  "AoC day 13"
  []
  (->> (parse-patterns (slurp "./input.txt"))
       ; It's pointless/I can't be bothered (choose one) to implement finding symmetry horizontally, so instead we
       ; rotate it clockwise and find it vertically again.
       (map #(+ (find-vert-symmetry (rotate-clockwise %) nil)
                (* (find-vert-symmetry % nil) 100)))
       (reduce +)))

(defn flip-symbol [s] (if (= "." s) "#" "."))

(defn try-all-combos
  "Try every combination of patt with one character flipped until we get a new line of symmetry"
  [patt original-solution]
  (set (for [row (range 0 (count patt))
             col (range 0 (count (first patt)))
             :let [match (find-vert-symmetry (update-in patt [row col] flip-symbol) original-solution)]
             :when (not= 0 match)]
         match)))

(defn part-two
  "Brute force this sucker."
  []
  (->> (parse-patterns (slurp "./input.txt"))
       (map (fn [patt]
              (let [original-solution-vert (find-vert-symmetry patt nil)
                    original-solution-horz (find-vert-symmetry (rotate-clockwise patt) nil)]
                (+ (or (first (try-all-combos (rotate-clockwise patt) original-solution-horz)) 0)
                   (* (or (first (try-all-combos patt original-solution-vert)) 0) 100)))))
       (reduce +)))