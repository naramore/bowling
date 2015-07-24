(ns bowling.core-test
  (:require [clojure.test :refer :all]
            [bowling.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.math.combinatorics :as combo]
            [clojure.test.check.clojure-test :refer :all]))

(def check-num 1000)

(def bad-rolls
    (->> (range 0 10)
         (#(combo/selections % 2))
         (filter #(< (apply + %) 10))))

(def spare-rolls
    (->> (range 0 11)
         (#(combo/selections % 2))
         (filter #(and (not= (first %) 10)
                       (= (apply + %) 10)))))

(defn simulate-bowling-game [rolls]
    (->> (map bowling.core/roll! rolls)
         last
         (bowling.core/score-game)))

(defn bowling-game-valid [expected actual]
    (and (= expected actual)
         (nil? (bowling.core/roll! 0))))

(defn game-verifier [roll-sel roll-cal sum-fn]
    (prop/for-all [game (gen/vector (gen/elements roll-sel) 10)]
        (let [begin (bowling.core/start-game!)
              rolls (roll-cal game)
              sum (sum-fn rolls)
              result (simulate-bowling-game rolls)]
            (bowling-game-valid sum result))))

(defspec bad-game-scores-correctly
    check-num
    (game-verifier bad-rolls
                   (partial apply concat)
                   (partial apply +)))

(defspec all-spare-game-scores-correctly
    check-num
    (game-verifier spare-rolls
                   #(->> (apply concat %)
                         vec
                         ((fn [coll] (conj coll 5))))
                   #(->> (partition 3 2 %)
                         (apply concat)
                         (apply +))))

(deftest all-strikes-score-three-hundred
    (let [begin (bowling.core/start-game!)
          rolls (repeat 12 10)
          final-score 300
          result (simulate-bowling-game rolls)]
        (is (bowling-game-valid final-score result))))