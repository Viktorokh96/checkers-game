(import PIL [Image])
(import location [rloc-to-tuple to-rel-loc])

(defn/a draw-board [brd]
    (let [
        d-chs (:checkers brd)
        bg-path "/home/vokhotnikov/Misc/pygames/checkers/docs/ch_bg.jpg"
        black-s "/home/vokhotnikov/Misc/pygames/checkers/docs/black_ch.jpg"
        black-q "/home/vokhotnikov/Misc/pygames/checkers/docs/black_ch_q.jpg"
        white-s "/home/vokhotnikov/Misc/pygames/checkers/docs/white_ch.jpg"
        white-q "/home/vokhotnikov/Misc/pygames/checkers/docs/white_ch_q.jpg"
        result-img-path "./current_turn.png"

        bg (.open Image bg-path)
        abs-loc-to-crd (fn [aloc]
            (let [[x y] (rloc-to-tuple (to-rel-loc aloc "white"))]
                #((+ 20 (* 48 (- x 1))) (+ 20 (* 48 (- 7 (- y 1)))))
            )
        )
        img-by-ch (fn [ch] 
            (.open Image (get {
                #("white" "simple") white-s
                #("white" "queen") white-q
                #("black" "simple") black-s
                #("black" "queen") black-q
            } #((:team ch) (:type ch))))
        )
    ]
        (for [ch (.values d-chs)]
            (.paste bg (img-by-ch ch) (abs-loc-to-crd (:aloc ch)))
        )
        (.save bg result-img-path)

        result-img-path
    )
)
