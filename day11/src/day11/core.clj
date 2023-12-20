(ns day11.core
  (:gen-class)
  (:require
    [clojure.string :as string]
    [clojure.math.combinatorics :as combo])
  (:import (java.util Collection Vector)))

(defn make-space
  "Turn the raw input string into a 2D vector"
  [s]
  (into [] (map #(string/split % #"") (string/split-lines s))))

(defn filter-only-empty
  "Given a function to extract rows/columns by index, return the indices corresponding to empty rows/columns"
  [cnt extract-fn]
  (into [] (filter (comp not nil?)
                   (for [line (range 0 cnt)]
                     (if (neg? (.indexOf (extract-fn line) "#")) line nil)))))

(defn find-expanded-space
  "Get a map of rows and columns that are empty and therefore should count for two due to expansion"
  [space]
  {:rows    (filter-only-empty (count space) #(space %))
   :columns (filter-only-empty (count (space 0)) (fn [c] (map #(% c) space)))})

(defn find-galaxies
  "Return a vector of [x y] coordinates of galaxies within the space map"
  [space]
  (loop [line 0
         idx 0
         galaxies []]
    (cond
      (>= line (count space)) galaxies
      (>= idx (count (space line))) (recur (inc line) 0 galaxies)
      :else (let [idx (.indexOf (Vector. ^Collection (space line)) "#" idx)]
              (if (neg? idx)
                (recur (inc line) 0 galaxies)
                (recur line (inc idx) (conj galaxies [idx line])))))))

(defn measure-distance
  "Measure the distance between two [x y] pairs, accounting for any rows or columns of expanded space crossed"
  [expanded-space expansion-factor [g1x g1y] [g2x g2y]]
  (let [raw-distance (+ (abs (- g2x g1x)) (abs (- g2y g1y)))
        crossed-exp-rows (count (filter #(<= (min g1y g2y) % (max g1y g2y)) (expanded-space :rows)))
        crossed-exp-columns (count (filter #(<= (min g1x g2x) % (max g1x g2x)) (expanded-space :columns)))]
    (+ raw-distance (* crossed-exp-rows (dec expansion-factor)) (* crossed-exp-columns (dec expansion-factor)))))

(defn -main
  "AoC day 11"
  [& args]
  (let [space (make-space (slurp "./input.txt"))
        expanded-space (find-expanded-space space)]
    (reduce +
            (for [[galaxy-1 galaxy-2] (combo/combinations (find-galaxies space) 2)]
              (measure-distance expanded-space 2 galaxy-1 galaxy-2)))))

(defn part-two
  "As part one but empty space is 1,000,000 times bigger"
  []
  (let [space (make-space (slurp "./input.txt"))
        expanded-space (find-expanded-space space)]
    (reduce +
            (for [[galaxy-1 galaxy-2] (combo/combinations (find-galaxies space) 2)]
              (measure-distance expanded-space 1000000 galaxy-1 galaxy-2)))))
