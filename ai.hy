; AI module
(import game [do-turn])
(import random [choices])
(import utils [flatten])

(defn explore-f-moves [orig-mv game r-depth [mv-to-expl None]]
    (let [
        orig-team (:team orig-mv)
        mv-to-expl (or mv-to-expl orig-mv)
        game-after-move (do-turn game mv-to-expl)
        turn-team-after-move (:turn game-after-move)
        board-after-move (:board game-after-move)
        am-after-move (get (:available-movements board-after-move) turn-team-after-move)
    ]
        (if (> r-depth 0)
            (lfor mv am-after-move 
                (explore-f-moves orig-mv game-after-move (- r-depth 1)
                    :mv-to-expl mv))
            [(get (:scores board-after-move) orig-team)]
        )
    )
)

(defn probabilistic-monster-ai [game current-turn [res-depth 2]]
    (let [
        ; list[movement]
        board (:board game)
        my-team current-turn
        resolution-depth res-depth
        am (
            get (:available-movements board) current-turn
        )
        avg (fn [scores] 
            (if (> (len scores) 0)
                (/ (sum scores) (len scores))
                0
            )
        )
        av-mv (lfor m am [
            (avg (flatten (explore-f-moves m game resolution-depth))) m
        ])
        [avgs moves] (if av-mv 
            (list (zip #* av-mv)) [None None])
        rational-degree 4
        normalized-avgs (when avgs 
            (lfor avg avgs (** (+ 1 (- avg (min (or avgs [0.0])))) rational-degree)))
        choosed-mv (when normalized-avgs 
            (get (choices moves :weights normalized-avgs :k 1) 0))
    ]
        (and am choosed-mv)
    )
)