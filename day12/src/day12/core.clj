(ns day12.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn replace-at
  "Return s where the character at idx is replaced by replacement"
  [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn process-lines
  "Create a map of pattern and sequence for each input line"
  [s]
  {:pattern (first (re-seq #"[?#.]+" s))
   :seq     (mapv read-string (re-seq #"\d+" s))})

; Repeat pattern 5 times, separated by ?, and the sequence 5 times
(defn pentuple-entry
  [entry]
  {:pattern (string/join "?" (take 5 (repeat (entry :pattern))))
   :seq     (into [] (apply concat (take 5 (repeat (entry :seq)))))})

(defn chars-remaining-in-str [string curr] (- (count string) curr))
(defn chars-required-for-seq [seq] (+ (reduce + seq) (dec (count seq))))

(def count-possibilities
  ; Memoizing the function makes part two complete in reasonable time because common sub-patterns will normally be
  ; recalculated a hell of a lot. For example, consider the pattern ?..??..?.. 2,1
  ; For the first step, we will compute ...?..?..? and #..?..?..?. The second call will have different values for
  ; chain but, because the first # can't match and the second character is ., the remaining computations are identical
  ; for both branches onwards. Therefore, we only need to actually compute it once.
  (memoize
    (fn [patt curr seq chain just-matched?]
      "Recursively work out possibile combinations by considering substrings of the pattern, starting from the whole
      thing and moving the start character rightwards. When a partial solution is identified as invalid, it is
      immediately terminated so no further processing must be done.

      - patt: The input pattern
      - curr: The current index in patt being considered
      - seq: The remaining subsequence of broken springs to be matched
      - chain: The running total of broken springs we've encountered since the last working one
      - just-matched?: Did the previous iteration yield a match? Used to enforce at non-contiguity (?) between sequences

      I can't take any credit for this; my initial solution to part one was a brute-force where I tried to be clever by
      only enumerating possibilities where the total count of broken springs did not exceed the known maximum.
      It never occurred to me that this should be solved recursively; this reddit comment put me on the right path:

      https://www.reddit.com/r/adventofcode/comments/18ghux0/comment/kd0npmi/"
      (cond
        ; If we get to the end of the sequence, we've found a match if there are no more known-broken springs...
        (empty? seq) (if (string/index-of patt "#" curr) 0 1)
        ; ...but if we no longer have enough characters in the patt, we can't match
        (< (chars-remaining-in-str patt curr) (- (chars-required-for-seq seq) chain)) 0
        :else (let [char (subs patt curr (inc curr))]
                (cond
                  ; If the spring is working, there are two possibilities:
                  ; - We haven't encountered any broken springs since the last match, so move on
                  ; - We _have_, but the last iteration didn't match the next sequence, so this is a dead end
                  (= "." char) (if (= chain 0) (count-possibilities (subs patt (inc curr)) 0 seq 0 false) 0)
                  ; If the previous character formed a match but this spring is broken too, this whole branch is dead
                  ; because sequences need to be separated by at least one working spring
                  (= "#" char) (if just-matched?
                                 0
                                 (if (= (inc chain) (first seq))
                                   ; If we've matched the sequence remove that from seq and check the next chunk of patt
                                   (count-possibilities (subs patt (inc curr)) 0 (subvec seq 1) 0 true)
                                   ; Otherwise look at the next character to see if we can eventually match a sequence
                                   (count-possibilities patt (inc curr) seq (inc chain) false)))
                  ; If its unknown, return the total number of combinations from both possibilities
                  :else (+ (count-possibilities (replace-at patt curr "#") curr seq chain just-matched?)
                           (count-possibilities (replace-at patt curr ".") curr seq chain just-matched?))))))))

(defn -main
  "AoC day 12"
  []
  (->> (map process-lines (string/split-lines (slurp "./input.txt")))
       (map #(count-possibilities (% :pattern) 0 (% :seq) 0 false))
       (reduce +)))

(defn part-two
  []
  (->> (map process-lines (string/split-lines (slurp "./input.txt")))
       (map pentuple-entry)
       (map #(count-possibilities (% :pattern) 0 (% :seq) 0 false))
       (reduce +)))