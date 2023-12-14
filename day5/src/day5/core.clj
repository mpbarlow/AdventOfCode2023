(ns day5.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn extract-seeds
  "Extract a list of seed numbers from top line of the mapping"
  [s]
  (map (comp read-string first) (re-seq #"(\d+)" (re-find #"seeds: (?:\d+\s?)+" s))))

(defn get-section-title
  "Return the mapping if s is a section title (e.g. 'foo-to-bar map'), else nil"
  [s]
  (last (re-find #"([a-z-]+) map:" s)))

(defn extract-maps
  "Extract each source-destination mapping into a map keyed by the section title"
  [lines]
  (loop [[line & remaining] lines
         current-key nil
         m {}]
    (if (empty? remaining)
      m
      (let [section-title (get-section-title line)]
        (if (and current-key (not section-title) (not= line ""))
          ; If we have a key to assign it to, and we're not on a new section header or a blank line, add the
          ; mappings into the current section
          (recur remaining current-key (update-in m [(keyword current-key)] conj (map read-string (string/split line #" "))))
          ; Otherwise carry on, setting the section title (which may be nil)
          (recur remaining section-title m))))))

(defn get-match
  [goal mapping]
  (first (for [[dest src r] mapping
               :when (and (>= goal src) (<= goal (+ src (- r 1))))]
           (+ dest (- goal src)))))

(defn follow-path
  [maps seed path]
  (loop [source seed
         [current & remaining] path]
    (if (= current nil)
      source
      (let [mapping (current maps)
            destination (get-match source mapping)]
        ; Anything not in the map maps to a destination equal to the source
        (recur (or destination source) remaining)))))

(def path [:seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light :light-to-temperature :temperature-to-humidity :humidity-to-location])

(defn find-closest-for-seeds
  [seeds maps]
  (->> seeds
       (map #(follow-path maps % path))
       (apply min)))

(defn get-leading-section
  "Given a goal start and end value, find the section that leads before the start point of the next mapped section.
  By definition this is unmapped, so the destination = the source -- no need to translate anything."
  [g-start g-end start]
  (if (> g-start start)
    [nil nil]
    [g-start (min (- start 1) g-end)]))

(defn get-overlapping-section
  "The section of goal-start...goal-end that overlaps with start...end is a little trickier. We need to account for
  either range ending first, and we also need to translate from source to destination values."
  [g-start g-end source-start source-end]
  (if (or (> source-start g-end) (> g-start source-end))
    [nil nil]
    [(max source-start g-start) (min source-end g-end)]))

(defn translate-subrange-to-destination
  "Given the original source to destination mapping, and the start and end source values we matched to, return the
  corresponding start and end destination values."
  [dest original-start [matched-start matched-end]]
  (if (nil? matched-start)
    [nil nil]
    (let [subrange-start (- matched-start original-start)
          new-dest (+ dest subrange-start)]
      [new-dest (+ new-dest (- matched-end matched-start))])))

(defn extract-subranges
  "Break the goal [start end] we are looking for into a series of subranges, where each subrange either:
  - matches a mapping range in sorted-mappings, for which we can return the corresponding destination range,
  - or does not match a mapping, in which case we take the destination range to be the same as the source
  Before passing anything to this, convert the range to the last value, cause it makes life way easier.
  The mappings also must be sorted by their source values."
  [[g-start g-end] sorted-mappings]
  (loop [g-start g-start
         [[dest start end] & remaining] sorted-mappings
         subranges []]
    (if (>= g-start g-end)
      subranges
      (if (nil? dest)
        ; Once we've ran out of mapping to process, if we have range left over we add that as one final subrange.
        (conj subranges [g-start g-end])
        (let [leading-goal-section (get-leading-section g-start g-end start)
              overlapping-section (get-overlapping-section g-start g-end start end)
              translated-overlap (translate-subrange-to-destination dest start overlapping-section)
              max-match (max (or (get leading-goal-section 1) 0) (or (get overlapping-section 1) 0))]
          (recur
            ; Start the next iteration from the max point we were able to match, or try again against the next map range
            ; if we were not able to map anything at all
            (if (> max-match 0) (+ max-match 1) g-start)
            remaining
            (apply conj subranges (remove #(nil? (get % 0)) [leading-goal-section translated-overlap]))))))))

; Convert mappings to start end instead of start range
; Sort mappings by source
(def seed-ranges
  (->> (slurp "./input.txt")
       string/split-lines
       first
       extract-seeds
       (partition 2)
       (map (fn [[start range]] [start (+ start (- range 1))]))))

(def sorted-maps
  (into {} (for [[key mappings] (->> (slurp "./input.txt")
                                     string/split-lines
                                     extract-maps)]
             [key (sort-by
                    #(get % 1)
                    <
                    (map (fn [[dest source range]] [dest source (+ source (- range 1))]) mappings))])))

(defn follow-path-ranged
  [seed-ranges sorted-maps path]
  (loop [[next & remaining] path
         subranges seed-ranges]
    (let [next-subranges (apply concat (map #(extract-subranges % (next sorted-maps)) subranges))]
      (if (empty? remaining)
        next-subranges
        (recur remaining next-subranges)))))

(defn part2
  []
  (apply min (map first (follow-path-ranged seed-ranges sorted-maps path))))

(defn -main
  "AoC day 5"
  [& args]
  (let [lines (string/split-lines (slurp "./input.txt"))
        seeds (extract-seeds (first lines))
        maps (extract-maps lines)]
    (find-closest-for-seeds seeds maps)))
