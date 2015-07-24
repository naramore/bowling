(ns bowling.core)

;; Game Data Structure:
;; ---------------------
;; Frame #                                                       (optional)
;;    1     2     3     4     5     6     7     8     9     10    11    12
;; [[# #] [# #] [# #] [# #] [# #] [# #] [# #] [# #] [# #] [# #] [# #] [# #]]

(def pins-per-frame 10)

(def frames-per-game 10)

(defn- spare? [frame]
    (= (apply + frame) pins-per-frame))

(defn- strike? [frame]
    (and (= (first frame) pins-per-frame)
         (empty? (rest frame))))

(defn- next-n-rolls-exist? [n frame-group]
    (->> (rest frame-group)
         (apply concat)
         count
         (<= n)))

(defn- score-n-rolls [n frame-group]
    (->> (rest frame-group)
         (apply concat)
         (take n)
         (apply +)))

(defn- extra-rolls [frame]
    (+ (if (spare? frame) 1 0)
       (if (strike? frame) 1 0)))

(defn- score-frame [frame-group]
    (let [this-frame (first frame-group)]
        (+ (apply + this-frame)
           (score-n-rolls (extra-rolls this-frame)
                          frame-group))))

(defn- frame-score-complete? [frame-group]
    (next-n-rolls-exist? (extra-rolls (first frame-group))
                         frame-group))

(defn- frame-complete? [frame]
    (or (= (first frame) pins-per-frame)
        (= (count frame) 2)))

(defn- is-game-finished? [frames]
    (let [frame-good? #(and (frame-score-complete? %)
                            (frame-complete? (first %)))]
        (->> (take frames-per-game (partition-all 3 1 frames))
             (map frame-good?)
             (apply =)
             (and (>= (count frames) frames-per-game)))))


(def game-state (atom nil))

(defn start-game! []
    (reset! game-state [[]]))

(defn stop-game! []
    (reset! game-state nil))

(defn roll! [pins-hit]
    (if (is-game-finished? @game-state)
        nil
        (do
            (cond (frame-complete? (peek @game-state))
                  (swap! game-state conj []))
            (swap! game-state
                   #(conj (pop %) (conj (peek %) pins-hit))))))

(defn score-game [frames]
    (let [frame-groups (take frames-per-game (partition-all 3 1 frames))
          frame-scores (map score-frame frame-groups)]
        (apply + frame-scores)))
