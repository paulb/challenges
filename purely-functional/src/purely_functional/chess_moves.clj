(ns purely-functional.chess-moves)

;; Chess moves

;; Write a function that determines if a chess piece, on an empty board, can move from one space to another in one move.

;; Examples

;; (can-move? :pawn "A2" "A3") ;=> true
;; (can-move? :queen "H1" "A8") ;=> true
;; (can-move? :knight "A4" "A5") ;=> false ;; (that's not how knights move)
;; (can-move? :king "A8" "A9") ;=> false ;; (that's off the board)

(def ^:private rows [\A \B \C \D \E \F \G \H])
(def ^:private columns [\1 \2 \3 \4 \5 \6 \7 \8])
(def ^:private boundaries (set (concat rows columns)))

(def ^:private board
  (set (for [row rows
             column columns]
         (str row column))))

(def ^:private rules
  {:pawn #{{:direction :forward :count 1}}
   :queen #{{:direction :forward :count 7}
            {:direction :backward :count 7}
            {:direction :sideways :count 7}
            {:direction :diagonal :count 7}}
   :king #{{:direction :forward :count 1}
           {:direction :backward :count 1}
           {:direction :sideways :count 1}
           {:direction :diagonal :count 1}}})

(defn- move
  [start destination]
  (let [[columns rows] (map vector start destination)
        columns (map int columns)
        rows (map #(Character/digit % 10) rows)
        move-count (- (second rows) (first rows))]
    {:direction ;; if a to a, forward or backward depending on count pos or neg.
                ;; if a to b, sideways or diagonal forward/backward depending.
                ;; currently assumes a move of some kind, staying in place will fail.
                ;; knight move?
                (cond
                  (= (first columns) (second columns))
                  (if (pos? move-count) :forward :backward)

                  (not= (first columns) (second columns))
                  (if (zero? move-count) :sideways :diagonal))
     :count (Math/abs move-count)}))

(defn can-move?
  [piece start destination]
  (let [{:keys [direction count] :as move*} (move start destination)
        [column row] destination]
    (boolean (and (boundaries column)
                  (boundaries row)
                  (some #(and (= (:direction move*) (:direction %))
                              (<= (:count move*) (:count %)))
                        (rules piece))))))

;; Tests
;; TODO More (including no move?)

(defn pawn-can-move-forward
  []
  (assert (= (can-move? :pawn "A2" "A3") true)))

(defn pawn-cannot-move-sideways
  []
  (assert (= (can-move? :pawn "A2" "B3") false)))

(defn pawn-cannot-move-diagonally
  []
  (assert (= (can-move? :pawn "A2" "B3") false)))

(defn queen-can-move-forward
  []
  (assert (= (can-move? :queen "A1" "A8") true)))

(defn queen-can-move-diagonally
  []
  (assert (= (can-move? :queen "H1" "A8") true))
  (assert (= (can-move? :queen "A1" "C3") true)))

(defn knight-cannot-move-forward
  []
  (assert (= (can-move? :knight "A4" "A5") false)))

(defn king-cannot-move-off-the-board
  []
  (assert (= (can-move? :king "A8" "A9") false)))

(defn run-chess-tests
  []
  (pawn-can-move-forward)
  (pawn-cannot-move-sideways)
  (pawn-cannot-move-diagonally)
  (queen-can-move-forward)
  (queen-can-move-diagonally)
  (knight-cannot-move-forward)
  (king-cannot-move-off-the-board))
