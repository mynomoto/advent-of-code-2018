(ns advent-of-code-2018.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn read-input
  [filename]
  (-> (io/resource filename)
      slurp))

(defn read-line-input
  [filename]
  (-> (read-input filename)
      str/split-lines))

(def day1-input (read-line-input "day01-input"))

(defn day1-part1
  [day1-input]
  (->> day1-input
       (map #(Integer/parseInt %))
       (apply +)))

(defn day1-part2
  [day1-input]
  (->>
    day1-input
    (map #(Integer/parseInt %))
    cycle
    (reduce (fn [{:keys [:frequency :seen]} i]
              (let [new-frequency (+ frequency i)]
                (if (contains? seen new-frequency)
                  (reduced new-frequency)
                  {:frequency new-frequency, :seen (conj seen new-frequency)})))
      {:frequency 0, :seen #{0}})))

(def day2-input (read-line-input "day02-input"))

(defn day2-part1
  [day2-input]
  (let [freq (map #(-> %
                       frequencies
                       vals
                       set)
               day2-input)
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
  [day2-input]
  (let [words (sort day2-input)]
    (reduce (fn [words candidate]
              (if-some [result (check-prototype? candidate words)]
                (reduced (apply remove-difference "" result))
                (rest words)))
      (rest words)
      words)))

(def day3-input (read-line-input "day03-input"))

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
  [day3-input]
  (->> (map claim->coordinates day3-input)
       (reduce (fn [{:keys [:union :intersection]} [_ s]]
                 (let [new-intersection (set/union (set/intersection union s)
                                                   intersection)
                       new-union (set/union union s)]
                   {:union new-union, :intersection new-intersection}))
         {:union #{}, :intersection #{}})
       :intersection))

(defn day3-part1 [day3-input] (count (claim-intersections day3-input)))

(defn day3-part2
  [day3-input]
  (let [all-intersections (claim-intersections day3-input)]
    (some (fn [[id s]]
            (when (empty? (set/intersection all-intersections s)) id))
          (map claim->coordinates day3-input))))

(def day4-input (read-line-input "day04-input"))

(defn parse-record
  [record]
  (let [[date time] (str/split (subs record 1 17) #" ")
        [hour minute] (str/split time #":")
        message (subs record 19)
        parsed-message (cond (str/starts-with? message "Guard")
                               (let [[_ id & rest] (str/split message #" ")]
                                 {:type :guard, :id id})
                             (str/starts-with? message "falls") {:type :sleeps}
                             (str/starts-with? message "wakes") {:type :wakes})]
    (assoc parsed-message
      :date date
      :hour hour
      :minute (Integer/parseInt minute))))

(defn sleep-aggregate
  [input]
  (->>
    input
    sort
    (map parse-record)
    (partition-by (fn [r] (= :guard (:type r))))
    (partition 2)
    (map
      (fn [[guard sleep-records]]
        (let [guard (-> guard
                        first
                        :id)
              sleep-records (partition 2 sleep-records)
              sleep-summary (mapv (fn [[start end]]
                                    {:start (:minute start),
                                     :total (- (:minute end) (:minute start)),
                                     :minutes-slept (range (:minute start)
                                                           (:minute end)),
                                     :end (:minute end)})
                              sleep-records)]
          [guard sleep-summary])))
    (group-by first)
    (reduce (fn [acc [k v]] (assoc acc k (apply concat (mapv second v)))) {})))

(defn day4-part1
  [day4-input]
  (let [aggregate (sleep-aggregate day4-input)
        totals
          (reduce (fn [acc [k v]]
                    (assoc acc
                      k (reduce (fn [acc sleep] (+ acc (:total sleep))) 0 v)))
            {}
            aggregate)
        max-sleep (reduce max 0 (vals totals))
        total->id (set/map-invert totals)
        max-sleep-id (total->id max-sleep)
        all-sleep-choosen-guard (get aggregate max-sleep-id)
        freq (frequencies (mapcat #(range (:start %) (:end %))
                            all-sleep-choosen-guard))
        max-frequency (reduce max 0 (vals freq))
        frequency->minute (set/map-invert freq)
        most-frequent-minute (frequency->minute max-frequency)
        guard-id-int (Integer/parseInt (subs max-sleep-id 1))]
    (* most-frequent-minute guard-id-int)))

(defn day4-part2
  [day4-input]
  (let [aggregate (sleep-aggregate day4-input)
        all-minutes
          (->> aggregate
               (reduce (fn [acc [k v]]
                         (let [minute->freq
                                 (frequencies
                                   (reduce (fn [acc sleep]
                                             (concat acc
                                                     (:minutes-slept sleep)))
                                     []
                                     v))
                               max-freq (reduce max 0 (vals minute->freq))
                               freq->minute (set/map-invert minute->freq)
                               minute-most-frequent (freq->minute max-freq)]
                           (conj acc
                                 {:frequencies minute->freq,
                                  :max-frequencies max-freq,
                                  :minute-most-frequent minute-most-frequent,
                                  :id k})))
                 []))
        selected-guard (apply max-key :max-frequencies all-minutes)
        guard-id-int (Integer/parseInt (subs (:id selected-guard) 1))]
    (* guard-id-int (:minute-most-frequent selected-guard))))

(def day5-input (read-input "day05-input"))

(defn toggle-case
  [c]
  (if (Character/isUpperCase c)
    (Character/toLowerCase c)
    (Character/toUpperCase c)))

(defn react
  [s]
  (reduce (fn [acc c]
            (if (= (toggle-case c) (last acc))
              (vec (butlast acc))
              (conj acc c)))
    []
    s))

(defn day5-part1
  [day5-input]
  (let [input (str/trim day5-input)] (count (react (seq input)))))

(defn day5-part2
  [day5-input]
  (let [input (str/trim day5-input)
        unit-types (-> input
                       str/lower-case
                       distinct)]
    (reduce min
      (count input)
      (for [ut unit-types]
        (->> input
             (remove #{ut (Character/toUpperCase ut)})
             react
             count)))))

(def day6-input (read-line-input "day06-input"))

(defn parse-coordinate
  [coordinate]
  (let [[x y] (str/split coordinate #", ")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn calculate-grid
  [coordinates]
  {:grid [(reduce (fn [acc [x _y]] (max acc x)) 0 coordinates)
          (reduce (fn [acc [_x y]] (max acc y)) 0 coordinates)],
   :coordinates coordinates})

(defn abs [n] (if (neg? n) (- n) n))

(defn manhattan-distance [[x1 y1] [x2 y2]] (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn find-nearest-coordinate
  [p coordinates]
  (reduce (fn [[_minimum-coordinate minimum-distance :as current] coordinate]
            (let [d (manhattan-distance p coordinate)]
              (cond (< d minimum-distance) [coordinate d]
                    (= d minimum-distance) [:tie d]
                    :else current)))
    [:none 1000000]
    coordinates))

(defn nearest-coordinate
  [{:keys [:grid :coordinates], :as data}]
  (let [[max-x max-y] grid]
    (assoc data
      :nearest (for [x (range (inc max-x))
                     y (range (inc max-y))]
                 [[x y] (find-nearest-coordinate [x y] coordinates)]))))

(defn remove-infinity-and-ties
  [{:keys [:grid :nearest], :as data}]
  (let [[max-x max-y] grid
        to-remove (->> nearest
                       (filter (fn [[[x y] _]]
                                 (or (= x 0) (= y 0) (= x max-x) (= y max-y))))
                       (map (fn [[_ [p _]]] p))
                       set)]
    (update data
            :nearest
            #(remove (fn [[_ [p _]]] (contains? to-remove p)) %))))

(defn day6-part1
  [day6-input]
  (->> day6-input
       (map parse-coordinate)
       calculate-grid
       nearest-coordinate
       remove-infinity-and-ties
       :nearest
       (map (fn [[_ [p _]]] p))
       frequencies
       (apply max-key second)
       second))

(defn total-distance
  [p coordinates]
  (apply + (map #(manhattan-distance % p) coordinates)))

(defn distance-to-all-coordinates
  [{:keys [:grid :coordinates], :as data}]
  (let [[max-x max-y] grid]
    (assoc data
      :distance (for [x (range (inc max-x))
                      y (range (inc max-y))]
                  [[x y] (total-distance [x y] coordinates)]))))

(defn day6-part2
  [day6-input]
  (->> day6-input
       (map parse-coordinate)
       calculate-grid
       distance-to-all-coordinates
       :distance
       (filter (fn [[p distance]] (< distance 10000)))
       count))

(def day7-input (read-line-input "day07-input"))

(defn parse-dependencies [s] (let [[_ p _ _ _ _ _ q] (str/split s #" ")] [p q]))

(defn add-all-steps
  [{:keys [:input], :as data}]
  (assoc data :all-steps (set (apply concat input))))

(defn add-available-steps
  [{:keys [:input :all-steps], :as data}]
  (let [steps-with-no-dependency (set/difference all-steps
                                                 (set (map second input)))]
    (assoc data :available-steps (sort steps-with-no-dependency))))


(defn run-step
  [{:keys [:input :dependency-requisites :all-steps :available-steps :steps],
    :as data}]
  (let [[first-step & other-steps] available-steps
        new-steps ((fnil conj []) steps first-step)
        new-steps-set (set new-steps)
        ; FIXME: can only add steps that have all requirements
        new-available-steps
          (set/difference
            (->> dependency-requisites
                 (filter (fn [[dependant dependencies]]
                           (empty? (set/difference dependencies
                                                   new-steps-set))))
                 (map (fn [[dependant _]] dependant))
                 (concat other-steps)
                 set)
            new-steps-set)]
    (assoc data
      :steps new-steps
      :available-steps (sort new-available-steps))))

(defn run-all-steps
  [data]
  (if (empty? (:available-steps data)) data (recur (run-step data))))

(defn add-dependency-requisites
  [{:keys [:input], :as data}]
  (assoc data
    :dependency-requisites
      (->> input
           (group-by second)
           (reduce-kv (fn [acc k v] (assoc acc k (set (map first v)))) {}))))

(defn day7-part1
  [day7-input]
  (->> day7-input
       (map parse-dependencies)
       (hash-map :input)
       add-dependency-requisites
       add-all-steps
       add-available-steps
       run-all-steps
       :steps
       str/join))

(defn add-time-setup
  [{:keys [:all-steps], :as data}]
  (assoc data
    :required-time (reduce (fn [acc step]
                             (assoc acc step (- (int (first step)) 4)))
                     {}
                     all-steps)
    :current-time 0
    :workers {:w0 :idle, :w1 :idle, :w2 :idle, :w3 :idle, :w4 :idle}))

{:finish-at 61, :started-at 0, :task "A"}

(defn update-wip
  [{:keys [:current-time :workers], :as data}]
  (let [new-workers (reduce-kv (fn [acc k v]
                                 (let [new-v (cond (= :idle v) v)]
                                   (assoc acc k new-v)))
                               {}
                               workers)]
    (assoc data :workers new-workers)))

(defn find-idle-worker
  [workers]
  (cond (= :idle (:w0 workers)) :w0
        (= :idle (:w1 workers)) :w1
        (= :idle (:w2 workers)) :w2
        (= :idle (:w3 workers)) :w3
        (= :idle (:w4 workers)) :w4))

(defn allocate-work
  [{:keys [:current-time :workers :available-steps :required-time], :as data}]
  (if-some [available-worker (find-idle-worker workers)]
    (if (empty? available-steps)
      data
      (recur (-> data
                 (assoc-in
                   [:workers available-worker]
                   {:task (first available-steps),
                    :stated-at current-time,
                    :finish-at (+ current-time
                                  (get required-time (first available-steps)))})
                 (assoc :available-steps (rest available-steps)))))
    data))

(defn increment-time [data] (update data :current-time inc))

(defn run-time-step
  [data]
  (-> data
      update-wip
      allocate-work
      increment-time))

(defn run-all-steps-with-time
  [data]
  (if (= (set (:steps data)) (:all-steps data))
    data
    (recur (run-time-step data))))

(defn day7-part2
  [day7-input]
  (->> day7-input
       (map parse-dependencies)
       (hash-map :input)
       add-dependency-requisites
       add-all-steps
       add-available-steps
       add-time-setup
       run-time-step
       ; run-all-steps-with-time
       ; :steps
       ; str/join
    ))

(comment (day1-part1 day1-input)
         (day1-part2 day1-input)
         (day2-part1 day2-input)
         (day2-part2 day2-input)
         (day3-part1 day3-input)
         (day3-part2 day3-input)
         (day4-part1 day4-input)
         (day4-part2 day4-input)
         (day5-part1 day5-input)
         (day5-part2 day5-input)
         (day6-part1 day6-input)
         (day6-part2 day6-input)
         (day7-part1 day7-input)
         (clojure.repl/pst))
