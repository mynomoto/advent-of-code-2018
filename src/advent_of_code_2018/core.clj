(ns advent-of-code-2018.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn day1-part1
  []
  (-> (io/resource "day01-input")
      slurp
      str/split-lines
      (->>
        (map #(Integer/parseInt %))
        (apply +))))

(defn day1-part2
  []
  (-> (io/resource "day01-input")
      slurp
      str/split-lines
      (->>
        (map #(Integer/parseInt %))
        cycle
        (reduce (fn [{:keys [:frequency :seen]} i]
                  (let [new-frequency (+ frequency i)]
                    (if (contains? seen new-frequency)
                      (reduced new-frequency)
                      {:frequency new-frequency
                       :seen (conj seen new-frequency)}))) {:frequency 0
                     :seen #{0}}))))

