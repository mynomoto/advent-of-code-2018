(ns advent-of-code-2018.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn read-line-input
  [filename]
  (-> (io/resource filename)
      slurp
      str/split-lines))

(def day-1-input (read-line-input "day01-input"))

(defn day1-part1
  [day-1-input]
  (->> day-1-input
       (map #(Integer/parseInt %))
       (apply +)))

(defn day1-part2
  [day-1-input]
  (->>
    day-1-input
    (map #(Integer/parseInt %))
    cycle
    (reduce (fn [{:keys [:frequency :seen]} i]
              (let [new-frequency (+ frequency i)]
                (if (contains? seen new-frequency)
                  (reduced new-frequency)
                  {:frequency new-frequency, :seen (conj seen new-frequency)})))
      {:frequency 0, :seen #{0}})))

(def day-2-input (read-line-input "day02-input"))

(defn day2-part1
  [day-2-input]
  (let [freq (map #(-> %
                       frequencies
                       vals
                       set)
               day-2-input)
        twos (count (filter #(% 2) freq))
        threes (count (filter #(% 3) freq))]
    (* twos threes)))

(defn count-differences
  [acc s1 s2]
  (if (str/blank? s1)
    acc
    (let [char (subs s1 0 1)]
      (if (= char (subs s2 0 1))
        (recur acc (subs s1 1) (subs s2 1))
        (recur (inc acc) (subs s1 1) (subs s2 1))))))

(defn check-prototype?
  [candidate words]
  (loop [[attempt & r] words]
    (case (count-differences 0 candidate attempt)
      1 [candidate attempt]
      2 (recur r)
      nil)))

(defn remove-difference
  [acc s1 s2]
  (if (str/blank? s1)
    acc
    (let [char (subs s1 0 1)]
      (if (= char (subs s2 0 1))
        (recur (str acc char) (subs s1 1) (subs s2 1))
        (recur acc (subs s1 1) (subs s2 1))))))

(defn day2-part2
  [day-2-input]
  (let [words (sort day-2-input)]
    (reduce (fn [words candidate]
              (if-some [result (check-prototype? candidate words)]
                (reduced (apply remove-difference "" result))
                (rest words)))
      (rest words)
      words)))

(def day-3-input (read-line-input "day03-input"))

(defn claim->coordinates
  [claim]
  (let [[id rect] (-> claim
                      (str/split #" @ "))
        [start size] (str/split rect #": ")
        [start-x start-y] (str/split start #",")
        [size-x size-y] (str/split size #"x")
        start-x (Integer/parseInt start-x)
        start-y (Integer/parseInt start-y)
        size-x (Integer/parseInt size-x)
        size-y (Integer/parseInt size-y)]
    [id
     (set (for [x (range start-x (+ start-x size-x))
                y (range start-y (+ start-y size-y))]
            [x y]))]))

(defn claim-intersections
  [day-3-input]
  (->> (map claim->coordinates day-3-input)
       (reduce (fn [{:keys [:union :intersection]} [_ s]]
                 (let [new-intersection (set/union (set/intersection union s)
                                                   intersection)
                       new-union (set/union union s)]
                   {:union new-union, :intersection new-intersection}))
         {:union #{}, :intersection #{}})
       :intersection))

(defn day3-part1 [day-3-input] (count (claim-intersections day-3-input)))

(defn day3-part2
  [day-3-input]
  (let [all-intersections (claim-intersections day-3-input)]
    (some (fn [[id s]]
            (when (empty? (set/intersection all-intersections s)) id))
          (map claim->coordinates day-3-input))))

(comment (day1-part1 day-1-input)
         (day1-part2 day-1-input)
         (day2-part1 day-2-input)
         (day2-part2 day-2-input)
         (day3-part1 day-3-input)
         (day3-part2 day-3-input))
