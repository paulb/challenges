(ns kroo.bowling)

(defn score
  [frames]
  (loop [frame (first frames)
         remain (rest frames)
         score 0
         last-frame nil
         strike-count 0]
    (let [frame-score (+ (:roll1 frame) (:roll2 frame))
          bonus (cond
                  (= last-frame :spare) (:roll1 frame)
                  (= last-frame :strike) (if (= strike-count 1)
                                           frame-score
                                           (+ frame-score (:roll1 frame)))
                  :else 0)
          score (+ score frame-score bonus)
          last-frame (cond
                       (and (= frame-score 10) (not= (:roll2 frame) 0)) :spare
                       (and (= frame-score 10) (= (:roll2 frame) 0)) :strike
                       :else nil)
          strike-count (if (= last-frame :strike)
                         (inc strike-count)
                         0)]
      (if (seq remain)
        (recur (first remain) (rest remain) score last-frame strike-count)
        score))))

(defn expect-20
  []
  (let [game [{:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}]]
    (assert (= (score game) 20))))

(defn expect-29
  []
  (let [game [{:roll1 5 :roll2 5}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}]]
    (assert (= (score game) 29))))

(defn expect-30
  []
  (let [game [{:roll1 10 :roll2 0}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}]]
    (assert (= (score game) 30))))

(defn expect-49
  []
  (let [game [{:roll1 10 :roll2 0}
              {:roll1 10 :roll2 0}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}
              {:roll1 1 :roll2 1}]]
    (assert (= (score game) 49))))
