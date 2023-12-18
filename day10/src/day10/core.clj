(ns day10.core
  (:gen-class)
  (:require
    [clojure.string :as string]
    [clojure.set]))

; Map each symbol to which NESW direction it links to
(def symbol-map {"|" [true false true false]
                 "-" [false true false true]
                 "L" [true true false false]
                 "J" [true false false true]
                 "7" [false false true true]
                 "F" [false true true false]
                 "." [false false false false]})

(defn make-grid
  "Turn the raw file input into a 2D grid of tiles"
  [s]
  (into [] (map #(string/split % #"") (string/split-lines s))))

(defn find-start
  "Return the [x y] co-ord of the start location"
  [grid]
  (loop [line 0]
    (let [start-index (.indexOf (grid line) "S")]
      (if (> start-index -1)
        [start-index line]
        (recur (+ 1 line))))))

(defn get-possible-locations
  "Given a location and whether it's possible to move in each direction, return the possible next locations"
  [[x y] north? east? south? west?]
  (filter #(not= false %) [(and north? [x (- y 1)])
                           (and east? [(+ x 1) y])
                           (and south? [x (+ y 1)])
                           (and west? [(- x 1) y])]))

(defn get-next-location-for-s
  "S is a special case, we need to examine the squares around the tile rather than just the tile itself"
  [grid [x y]]
  (let [lookup (fn [x y idx] ((symbol-map ((grid y) x)) idx))
        north? (lookup x (- y 1) 2)
        east? (lookup (+ x 1) y 3)
        south? (lookup x (+ y 1) 0)
        west? (lookup (- x 1) y 1)]
    (first (get-possible-locations [x y] north? east? south? west?))))

(defn get-next-location
  "Get the next location for a given location that does not backtrack to the previous location"
  [grid [current-x current-y] last-location]
  (let [symbol ((grid current-y) current-x)]
    (if (= "S" symbol)
      (get-next-location-for-s grid [current-x current-y])
      (let [north? ((symbol-map symbol) 0)
            east? ((symbol-map symbol) 1)
            south? ((symbol-map symbol) 2)
            west? ((symbol-map symbol) 3)]
        (first (filter #(not= % last-location)
                       (get-possible-locations [current-x current-y] north? east? south? west?)))))))

(defn follow-loop
  "Follow the loop until we arrive back at the start location, returning the number of steps"
  [grid start-location]
  (loop [steps 0
         location start-location
         last-location [nil nil]]
    (if (and (= location start-location) (> steps 0))
      steps
      (recur (+ 1 steps) (get-next-location grid location last-location) location))))

(defn track-loop
  "Follow the loop until we arrive back at the start location, returning each point along the way"
  [grid start-location]
  (loop [points []
         location start-location
         last-location [nil nil]]
    (if (and (= location start-location) (not (empty? points)))
      points
      (recur (conj points location) (get-next-location grid location last-location) location))))

; Find the start point
; Follow the path until we get back to S
; The number of steps taken / 2 is the farthest point
(defn -main
  "AoC day 10"
  [& args]
  (let [grid (make-grid (slurp "./input.txt"))]
    (/ (follow-loop grid (find-start grid)) 2)))

(defn point-in-poly
  "Use raycasting to determine if [target-x y] is inside poly. Poly is a set of points defining the outline."
  [grid poly target-x y]
  ; If the target point is on an edge of the polygon, or on or outside the grid bounds, it can't be inside it and
  ; there's no need to cast a ray
  (if (or
        (>= y (- (count grid) 1))
        (>= target-x (- (count (grid y)) 1))
        (contains? poly [target-x y]))
    false
    (loop [x 0
           inside? false
           wall-opener nil]
      (if (> x target-x)
        inside?
        (let [tile ((grid y) x)
              ; Quite unsatisfying but I just looked at my input and know that my S is a |
              tile (if (= tile "S") "|" tile)]
          (if (or (= "-" tile) (not (contains? poly [x y])))
            ; Don't do anything special for tiles that aren't part of the loop, or are just slices of horizontal wall
            (recur (+ x 1) inside? wall-opener)
            ; The bit I didn't twig onto til I went to Reddit for hints: "S" bends swap from inside to outside, but "U"
            ; bends don't. I knew there was something differentiating horizontal slices of wall but couldn't quite
            ; grasp it.
            (case tile
              "|" (recur (+ x 1) (not inside?) nil)
              "F" (recur (+ x 1) inside? "F")
              "L" (recur (+ x 1) inside? "L")
              "7" (recur (+ x 1) (if (= wall-opener "L") (not inside?) inside?) nil)
              "J" (recur (+ x 1) (if (= wall-opener "F") (not inside?) inside?) nil))))))))

(defn draw-grid
  "Quick debug function to help test the part-two algorithm. Draws only the pipe segments in the main loop, underscores
  for points outside the loop and spaces for points inside it."
  []
  (let [grid (make-grid (slurp "./input.txt"))
        poly (set (track-loop grid (find-start grid)))]
    (filter nil? (for [y (range 0 140)
                       x (range 0 140)]
                   (do
                     (if (not (empty? (clojure.set/intersection poly (set [[x y]]))))
                       (print ((grid y) x))
                       (print (if (point-in-poly grid poly x y) "." " ")))
                     (when (= x 139) (print "\n"))
                     true)))))

(defn part-two
  []
  (let [grid (make-grid (slurp "./input.txt"))
        poly (set (track-loop grid (find-start grid)))
        bounding-box [[(reduce #(min %1 %2) (map first poly)) (reduce #(min %1 %2) (map last poly))]
                      [(reduce #(max %1 %2) (map first poly)) (reduce #(max %1 %2) (map last poly))]]]
    ; all x-y points inside the bounding box
    (count (filter true? (for [x (range (+ 1 ((bounding-box 0) 0)) ((bounding-box 1) 0))
                               y (range (+ 1 ((bounding-box 0) 1)) ((bounding-box 1) 1))]
                           ; Start just outside the min-x of the bounding box
                           (point-in-poly grid poly x y))))))
