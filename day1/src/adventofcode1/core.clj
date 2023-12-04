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

(defn first-and-last
  "Return a string resulting from the concatenation of the first and last elements of the input vector"
  [input]
  (str (first input) (last input)))

(defn convert-word-digit-to-digit
  "Convert e.g. 'one' to '1', 'three' to '3' etc."
  [input]
  (loop [remaining-replacements word-digit-map]
    (if (empty? remaining-replacements)
      input
      (let [[[search replacement] & remaining-replacements] (seq remaining-replacements)]
        (if (re-find (re-pattern (name search)) input)
          replacement
          (recur remaining-replacements))))))

(defn extract-digits
  "Extract all digits and word-digits from the input string, returning them in order"
  [input]
  (map convert-word-digit-to-digit
       (map last (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" input))))

(defn -main
  "AoC day one"
  [& args]
  (println
    (reduce +
            (map read-string
                 (map first-and-last
                      (map extract-digits
                           (string/split-lines (slurp "./aoc1_input"))))))))