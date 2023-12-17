(ns day8.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn lr-to-indices
  "Convert a series of LR directives to the corresponding index to look up"
  [s]
  (into [] (map #(if (= % "L") 0 1) (string/split s #""))))

(defn extract-directions
  "Extract location, left, and right from 'XXX = (YYY, ZZZ)'"
  [s]
  (let [[_ key left right] (first (re-seq #"([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" s))]
    [key left right]))

(defn make-map
  "Convert the sequence of directions into a map of left-right directions keyed by location"
  [lines]
  (into {} (map (fn
                  [line]
                  (let [[key left right] (extract-directions line)]
                    [key [left right]]))
                lines)))

(defn follow-path
  [start goal-fn path path-map]
  (loop [current start
         step 0
         path-loops 0]
    (if (>= step (count path))
      ; If we've exhausted our path, loop back to the beginning and increment the loops we've done
      (recur current 0 (+ 1 path-loops))
      (let [next (current (path step))]
        (if (goal-fn next)
          (+ (* path-loops (count path)) (+ 1 step))
          (recur (path-map next) (+ 1 step) path-loops))))))

(defn -main
  "AoC day 8"
  [& args]
  (let [lines (into [] (string/split-lines (slurp "./input.txt")))
        path (lr-to-indices (first lines))
        path-map (make-map (subvec lines 2))]
    (follow-path (path-map "AAA") #(= "ZZZ" %) path path-map)))

(defn part-two
  "Can't take any credit for this, went to Reddit for hints and saw that the inputs were engineered in a way to make
  the solution the least common multiple of the six individual paths. Meh."
  []
  (let [lines (into [] (string/split-lines (slurp "./input.txt")))
        path (lr-to-indices (first lines))
        path-map (make-map (subvec lines 2))
        starts (vals (select-keys path-map (filter #(string/ends-with? % "A") (keys path-map))))]
    (map (fn [s] (follow-path s #(string/ends-with? % "Z") path path-map)) starts)))