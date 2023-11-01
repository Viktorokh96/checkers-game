; Board location
; Relative location: "11" - "88", относительно представления одного из огроков
; Absolute location: "a1" - "h8"
(require hyrule [as->])
(import functools [cache])

(defn abs-loc [loc]
    loc
)

(defn [cache] rloc-to-tuple [rel-l]
    #((int (get rel-l 0)) (int (get rel-l 1)))
)

(defn [cache] rloc-in-up-line? [rel-l]
    (= "8" (get rel-l 1))
)

(defn [cache] to-rel-loc [abs-l team]
    (assert (in team ["white" "black"]))
    (let [direct-loc-transf (fn [al] (+ (str (- (ord (get al 0)) 96)) (get al 1)))]
        (if (= team "white")
            (direct-loc-transf abs-l)
            (as-> (direct-loc-transf abs-l) it 
                (map int it) 
                (map (fn [x] (- 9 x)) it) 
                (map str it) 
                (str.join "" it) 
            )
        )
    )
)

(defn [cache] to-abs-loc [rel-l team]
    (assert (in team ["white" "black"]))
    (let [direct-abs-transf (fn [rl] (+ (chr (+ 96 (int (get rl 0)))) (get rl 1)))]
        (if (= team "white")
            (direct-abs-transf rel-l)
            (as-> rel-l it 
                (map int it) 
                (map (fn [x] (- 9 x)) it) 
                (map str it) 
                (str.join "" it) 
                (direct-abs-transf it) 
            )
        )
    )
)

(defn [cache] in-field-rel? [rel-l]
    (if (is rel-l None) False
        (as-> rel-l it
            (map int it)
            (map (fn [l] (and (>= l 1) (<= l 8))) it)
            (all it)
        )
    )
)

(defn [cache] in-field-rel-or-none [rel-l]
    (if (in-field-rel? rel-l) rel-l None)
)

(defn [cache] in-field-abs? [abs-l]
    (in-field-rel? (to-rel-loc abs-l "white"))
)

(defn [cache] rel-move [rel-l x y]
    (in-field-rel-or-none (str.join "" [
        (str (+ (int (get rel-l 0)) x)) 
        (str (+ (int (get rel-l 1)) y))
    ]))
)

(defn [cache] rel-trans-dst-dir [rel-l dst dir / [following 0]]
    (let [
        [x y] dir
        dst (+ dst following)
    ]
        (rel-move rel-l (* dst x) (* dst y))
    )
)

(defn [cache] reversed-dir [dir]
    (tuple (map (fn [x] (* -1 x)) dir))
)

(defn [cache] dir-between-rloc [from-rloc to-rloc]
    (let [
        fl-t (rloc-to-tuple from-rloc)
        tl-t (rloc-to-tuple to-rloc)
        [fl-x fl-y] fl-t
        [tl-x tl-y] tl-t
        dx (- tl-x fl-x)
        dy (- tl-y fl-y)
    ]
        #((int (/ dx (abs dx))) (int (/ dy (abs dy))))
    )
)

(defn [cache] get-move-rays-iter [rel-loc / [dx -1] [dy +1] [initial-rloc None] [dst 1]]
    ; returns dict[loc, {:dst :dir}]
    ; where dist is max(abs(dx), abs(dy))

    (let [
        moved-rl (rel-move rel-loc dx dy)
        last-dir? (= [-1 -1] [dx dy])
        [next-dx next-dy] (get {
            #(-1 1) #(1 1)
            #(1 1) #(1 -1)
            #(1 -1) #(-1 -1)
            #(-1 -1) #(1 1)
        } #(dx dy))
        initial-rloc (or initial-rloc rel-loc)
    ]
        (cond 
            (in-field-rel? moved-rl) (
                | {moved-rl {"dst" dst "dir" #(dx dy)}} (
                    get-move-rays-iter moved-rl dx dy 
                        :initial-rloc initial-rloc 
                        :dst (+ dst 1)
                )
            )
            (and (not (in-field-rel? moved-rl)) (not last-dir?)) (
                get-move-rays-iter initial-rloc 
                    :dx next-dx :dy next-dy 
                    :initial-rloc initial-rloc
                    :dst 1
            )
            (and (not (in-field-rel? moved-rl)) last-dir?) {}
        )
    )
)

(defn [cache] get-move-rays [rel-loc]
    (get-move-rays-iter rel-loc)
)

(defn move-rays-select-by [rays / [by-dsts []] [by-dirs []]]
    (dfor [k v] (.items rays) 
        :if (and 
            (if (not by-dst) True (in (:dst v) by-dsts)) 
            (if (not by-dir) True (in (:dir v) by-dirs))
        )
        [k v]
    )
)

(assert (= "53" (to-rel-loc "e3" "white")))
(assert (= "55" (to-rel-loc "d4" "black")))

(assert (= "f7" (to-abs-loc "32" "black")))
(assert (= "a1" (to-abs-loc "88" "black")))
(assert (= "h8" (to-abs-loc "11" "black")))
(assert (= "c2" (to-abs-loc "32" "white")))

(assert (in-field-rel? "11"))
(assert (in-field-rel? "88"))
(assert (not (in-field-rel? "00")))
(assert (not (in-field-rel? "99")))

(assert (in-field-abs? "a1"))
(assert (in-field-abs? "h8"))
(assert (not (in-field-abs? "a0")))
(assert (not (in-field-abs? "h9")))

(assert (= "22" (rel-move "11" 1 1)))
(assert (= "45" (rel-move "34" 1 1)))
(assert (is None (rel-move "11" -1 -1)))
(assert (= "85" (rel-move "11" 7 4)))

;(print (get-move-rays "22"))
(assert (= {
    "13" {"dst" 1 "dir" #(-1 1)}
    "33" {"dst" 1 "dir" #(1 1)}
    "44" {"dst" 2 "dir" #(1 1)}
    "55" {"dst" 3 "dir" #(1 1)}
    "66" {"dst" 4 "dir" #(1 1)}
    "77" {"dst" 5 "dir" #(1 1)}
    "88" {"dst" 6 "dir" #(1 1)}
    "31" {"dst" 1 "dir" #(1 -1)}
    "11" {"dst" 1 "dir" #(-1 -1)}
} (get-move-rays "22")))

(assert (= "44" (rel-trans-dst-dir "22" 2 #(1 1))))
(assert (= "15" (rel-trans-dst-dir "33" 2 #(-1 1))))

(assert (= #(1 -1) (reversed-dir #(-1 1))))
(assert (= #(-1 -1) (reversed-dir #(1 1))))

(assert (= #(1 1) (dir-between-rloc "22" "33")))
(assert (= #(1 1) (dir-between-rloc "22" "44")))
(assert (= #(-1 1) (dir-between-rloc "33" "15")))