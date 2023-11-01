(import asyncio)
(import pprint [pprint])
(import board [get-initial-board applied-movement-to-board])
(import random [choice])
(import rules [who-is-winner?])
(import movement [abs-route-from-rel-route])
(import view [draw-board])

(defn wait-random-ai-movement-choice [game current-turn]
    (let [
        board (:board game)
        available-movements (get (:available-movements board) current-turn)
    ]
        (and available-movements (choice available-movements))
    )
)

(defn checkers-game [
    [board None] [turn "white"] / 
    [apply-movement None]
    [movement-sources {}] 
]
    (let [
        brd (or board (get-initial-board))
    ]
        (assert (in turn ["white" "black"]))
        {
            "meta" {"obj-type" "game"}
            "board" (applied-movement-to-board brd apply-movement)
            "turn" turn
            "winner" (who-is-winner? (:available-movements brd))
            "mvs" {
                "white" (:white movement-sources wait-random-ai-movement-choice)
                "black" (:black movement-sources wait-random-ai-movement-choice)
            }
        }
    )
)

(defn wait-cli-human-movement-choice [game current-turn]
    (let [
        board (:board game)
        available-movements (get (:available-movements board) current-turn)
        movements-d (dfor [idx m] (enumerate available-movements :start 1) [idx m])
    ]
        (when available-movements
            (get movements-d (int (input (+ 
                (str.join "\n" 
                (lfor [idx m] (.items movements-d) 
                    (+ 
                        (str idx) 
                        " " 
                        (str.join 
                            "-" 
                            (abs-route-from-rel-route (:relroute m) (:team m)))
                        " "
                        (str.join
                            "+"
                            (abs-route-from-rel-route (:swallowed-ch m) (:team m)))
                        ))) 
                    "\n:> "))))
        )
    )
)

(defn/a get-game-next-movement [game]
    (let [
        board (:board game)
        current-turn (:turn game)
        movement-choice-source (get (:mvs game) current-turn)
        mvs-is-async? (.iscoroutinefunction asyncio movement-choice-source)
        d-chs (:checkers (:board game))
        whites (lfor [aloc c] (.items d-chs) :if (= (:team c) "white") aloc)
        blacks (lfor [aloc c] (.items d-chs) :if (= (:team c) "black") aloc)
    ]
        (print "whites:" (str.join "," whites))
        (print "blacks:" (str.join "," blacks))
        (print "scores: " (:scores board))
        (if mvs-is-async? 
            (await (movement-choice-source game current-turn))
            (movement-choice-source game current-turn))
    )
)

(defn do-turn [game movement]
    (let [
        board (:board game)
        current-turn (:turn game)
        next-turn (if (= "white" current-turn) "black" "white")
    ]
        (checkers-game board next-turn
            :apply-movement movement
            :movement-sources (:mvs game)
        )
    )
)

(defn/a run-game [[configure-movement-src {}] [game None] [on-draw draw-board]]
    """Game loop (recursive way)"""
    (let [
        game (or game (checkers-game :movement-sources configure-movement-src))
        game (do-turn game (await (get-game-next-movement game)))
        winner (:winner game)
    ]
    (await (on-draw (:board game)))
    (cond 
        (= winner "white") (print "White team won!")
        (= winner "black") (print "Black team won!")
        (is winner None) (await (run-game 
            :configure-movement-src configure-movement-src 
            :game game))
    ))
)

(defn/a run-game-loop [[configure-movement-src {}] [game None] [on-draw draw-board]]
    """Game loop (not recursive way)"""
    (let [
        game (or game (checkers-game :movement-sources configure-movement-src))
        winner (:winner game)
    ]
    (while True
        (await (on-draw (:board game)))
        (setv winner (:winner game))
        (cond 
            (= winner "white") (do (print "White team won!") (break))
            (= winner "black") (do (print "Black team won!") (break))
            (is winner None) (setv game 
                (do-turn game (await (get-game-next-movement game))))
        ))
    )
)