(ns adventofcode1.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(def word-digit-map {:one   "1"
                     :two   "2"
                     :three "3"
                     :four  "4"
                     :five  "5"
                     :six   "6"
                     :seven "7"
                     :eight "8"
                     :nine  "9"})

(defn find-digit
  "Returns the first digit 0...9 found in input, or nil"
  [input]
  (re-find #"\d" input))

(defn find-word-digit
  "Searches input for a word digit and returns a vector containing the first matched digit and its index, or nil if
  none. First is according to order in word-digit-map, not in the string."
  [input]
  (loop [remaining-searches word-digit-map]
    (if (empty? remaining-searches)
      nil
      (let [
            [[search value] & remaining-searches] (seq remaining-searches)
            found-index (string/index-of input (name search))]
        (if found-index
          [value found-index]
          (recur remaining-searches))))))

(defn convert-string-to-digit-vector
  "Scan the input string and return a vector of all found digits and word-digits"
  [input]
  (loop [start 0
         to 1
         found-digits []]
    ; If we've reached the end of the string, return any digits we found
    (if (> to (count input))
      found-digits
      (let [found-digit (find-digit (subs input start to))]
        (if found-digit
          ; If we've found an actual digit, record it and move to the next character, as we know we can no longer find
          ; a word digit.
          (recur
            to
            (+ to 1)
            (conj found-digits found-digit))
          (let [[found-word-digit index] (find-word-digit (subs input start to))]
            (if found-word-digit
              ; If we find a word digit, reset the window to the second character of it and resume the search
              ; This ensures we get all digits in strings like "twoneight"
              (recur
                (+ start index 1)
                (+ start index 2)
                (conj found-digits found-word-digit))
              ; Otherwise widen the search to the next character
              (recur
                start
                (+ to 1)
                found-digits))))))))

(defn first-and-last
  "Return a string resulting from the concatenation of the first and last elements of the input vector"
  [input]
  (str (first input) (last input)))

(defn -main
  "AoC day one"
  [& args]
  (println
    (reduce +
            (map read-string
                 (map first-and-last
                      (map convert-string-to-digit-vector
                           (string/split-lines (slurp "./aoc1_input"))))))))