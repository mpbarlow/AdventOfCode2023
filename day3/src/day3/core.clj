(ns day3.core
  (:gen-class)
  (:require
    [clojure.string :as string]))

(defn re-seq-pos
  "Find the start and end index and match for each match of pattern in string.
  Shamelessly stolen from https://stackoverflow.com/a/21192299"
  [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (- (. m end) 1) :group (. m group)}
               (lazy-seq (step))))))))

(defn char-is-symbol
  [char]
  (and (not= char "")
       (not (re-find #"[0-9.]" char))))

(defn is-next-to-symbol-on-same-line
  [line start end]
  (or (char-is-symbol (str (get line (- start 1))))
      (char-is-symbol (str (get line (+ end 1))))))

(defn is-next-to-symbol-on-neighbouring-line
  [neighbouring-line start end]
  (not (empty? (for [p (range (- start 1) (+ end 2))
                     :when (char-is-symbol (str (get neighbouring-line p)))]
                 true))))

(defn is-next-to-symbol
  [match line prev-line next-line]
  (let [start (:start match)
        end (:end match)]
    (or (is-next-to-symbol-on-same-line line start end)
        (is-next-to-symbol-on-neighbouring-line prev-line start end)
        (is-next-to-symbol-on-neighbouring-line next-line start end))))

; Find asterisks
; Get numbers on same, prev, and next line
; Same line: filter by end = * -1 and start = * + 1
; prev/next line: filter by * between number start - 1 and number end + 1
; if != 2, discard
(defn get-numbers-on-same-line
  [line pos]
  (->> (re-seq-pos #"\d+" line)
       (filter #(or (= (- pos 1) (:end %))
                    (= (+ pos 1) (:start %))))
       (map :group)))

(defn get-numbers-on-neighbouring-line
  [neighbouring-line pos]
  (->> (re-seq-pos #"\d+" neighbouring-line)
       (filter #(and (>= pos (- (:start %) 1))
                     (<= pos (+ (:end %) 1))))
       (map :group)))

(defn get-numbers-connected-to-gears
  [gear-match line prev-line next-line]
  (let [pos (:start gear-match)]
    (flatten (conj (get-numbers-on-same-line line pos)
                   (get-numbers-on-neighbouring-line prev-line pos)
                   (get-numbers-on-neighbouring-line next-line pos)))))

(defn part-two
  []
  (let [lines (string/split-lines (slurp "./input.txt"))]
    (->> lines
         (map #(re-seq-pos #"\*" %))
         (map-indexed (fn [index matches]
                        (map
                          #(get-numbers-connected-to-gears % (get lines index) (get lines (- index 1)) (get lines (+ index 1)))
                          matches)))
         (apply concat)
         (filter #(= (count %) 2))
         (map #(map read-string %))
         (map (partial apply *))
         (reduce +))))

(defn -main
  "AoC day 3"
  [& args]
  (let [lines (string/split-lines (slurp "./input.txt"))]
    (->> lines
         (map #(re-seq-pos #"\d+" %))
         (map-indexed (fn [index matches]
                        (filter
                          #(is-next-to-symbol % (get lines index) (get lines (- index 1)) (get lines (+ index 1)))
                          matches)))
         (map #(map :group %))
         (map #(map read-string %))
         (map #(reduce + %))
         (reduce +))))
