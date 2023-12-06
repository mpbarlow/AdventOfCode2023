(ns day2.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn parse-round
  "Extract the number of cubes of each colour from a round string"
  [round-string]
  (let [red (last (re-find #"(\d+) red" round-string))
        green (last (re-find #"(\d+) green" round-string))
        blue (last (re-find #"(\d+) blue" round-string))]
    {:red red :green green :blue blue}))

(defn parse-game
  "Parse a game string into a map of the Game ID and the rounds, with the draws of each colour"
  [game-string]
  (let [[game & rest] (string/split game-string #":")
        game-no (last (re-find #"Game (\d+)" game))
        rounds (map parse-round (string/split (first rest) #";"))]
    {:game game-no :rounds rounds}))

(defn round-is-possible
  "Checks whether any colour exceeds the number of cubes we have"
  [round]
  (and
    (>= 12 (read-string (or (:red round) "0")))
    (>= 13 (read-string (or (:green round) "0")))
    (>= 14 (read-string (or (:blue round) "0")))))

(defn game-is-possible
  "Checks whether the game was possible by checking if every round was possible"
  [game]
  (every? true? (map round-is-possible (:rounds game))))

(defn min-colour
  "Grab the supplied colour from each game and return the maximum, i.e. the minimum needed for the game to be possible."
  [rounds-for-game colour]
  (->> rounds-for-game
       (map colour)
       (map (fn [r] (or r "0")))
       (map read-string)
       (apply max)))

(defn min-colours
  "For each colour :red, :green, and :blue in each round in a game, calculate the minimum of each needed to make the
  game possible."
  [rounds-for-game]
  (map (partial min-colour rounds-for-game) [:red :green :blue]))

(defn part-2
  "AoC day 2 part 2"
  []
  (->> (slurp "./input")
       string/split-lines
       (map parse-game)
       (map :rounds)
       (map (fn [r] (min-colours r)))
       (map (fn [r] (apply * r)))
       (reduce +)))

(defn -main
  "AoC day 2"
  [& args]
  (->> (slurp "./input")
       (string/split-lines)
       (map parse-game)
       (filter game-is-possible)
       (map :game)
       (map read-string)
       (reduce +)))