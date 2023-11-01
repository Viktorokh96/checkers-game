(import functools [partial])
(import checkers [
    checker checker-movement-applied checkers-view 
    d-checkers d-checkers-from-list
])
(import utils [assoc dissoc as-is])
(import pprint [pprint])
(import location [abs-loc])
(import movement [abs-route-from-rel-route])
(import rules [
    calculate-all-available-movements
    calculate-team-scores
])

(defn board [chs]
    {
        "meta" {"obj-type" "board"}
        "checkers" chs
        "available_movements" (calculate-all-available-movements chs)
        "scores" (calculate-team-scores chs)
    }
)

(defn board-updated [brd / [checkers-modifier as-is] #** changes]
    (board
        :chs (d-checkers 
            (:checkers changes (:checkers brd)) 
            :modifier checkers-modifier
        )
    )
)

(defn print-mv [mv]
    (print 
        (.format "{} move:" (:team mv)) 
        (str.join "-" (abs-route-from-rel-route (:relroute mv) (:team mv)))
        #* (if (:swallowed-ch mv) [
                "\t\tsw:"
                (str.join "+" (abs-route-from-rel-route (:swallowed-ch mv) (:team mv)))
            ] [])
        )
)

(defn applied-movement-to-board [brd mv]
    (if (and (is-not mv None) mv)
        (board-updated brd 
            :checkers-modifier (partial checker-movement-applied mv))
        brd
    )
)

(defn get-initial-board []
    (board (d-checkers-from-list [
                #* (lfor aloc [
                    "a1" "c1" "e1" "g1"
                    "b2" "d2" "f2" "h2"
                    "a3" "c3" "e3" "g3"
                ] (checker "simple" "white" (abs-loc aloc)))
                
                #* (lfor aloc [
                    "b8" "d8" "f8" "h8"
                    "a7" "c7" "e7" "g7"
                    "b6" "d6" "f6" "h6"
                ] (checker "simple" "black" (abs-loc aloc)))
            ]
        )
    )
)

; (pprint (get-initial-board))