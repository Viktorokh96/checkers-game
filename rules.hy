(require hyrule [as->])
(import functools [partial])
(import utils [first flatten])
(import checkers [checker d-checkers-from-list checkers-view])
(import movement [rel-route abs-route-from-rel-route movement swallowed-ch])
(import location [
    to-rel-loc to-abs-loc abs-loc in-field-rel?
    get-move-rays move-rays-select-by :as select-by
    rel-trans-dst-dir reversed-dir
])

(defn rloc-is-free? [rel-view rloc] 
    (and (is-not rloc None) (not-in rloc rel-view) (in-field-rel? rloc))
)

(defn get-following-free-rlocs [from-rloc rel-view dst dir / [following 1] [max-dst None]]
    "Return list of rlocs that is free to move"    
    (let
        [rloc (rel-trans-dst-dir from-rloc dst dir :following following)]
        (if (or (is max-dst None) (<= following max-dst))
            (if (rloc-is-free? rel-view rloc) 
                (+ [rloc] (get-following-free-rlocs from-rloc rel-view dst dir 
                            :following (+ 1 following) :max-dst max-dst))
                []
            )
            []
        )
    )
)

(defn get-enemies-swallowing-movements [ch d-chs / [from-movement None]]
    "Returns movements in which enemies will be swallowed"
    (let [
        ch-team (:team ch)
        rel-view (checkers-view d-chs ch-team)
        from-movement (or from-movement 
            (movement (rel-route [(:rloc ch)]) ch-team (swallowed-ch []))
        )
        from-rloc (:last-rel-point from-movement)
        c-rays (get-move-rays from-rloc)
        get-rloc (partial rel-trans-dst-dir from-rloc)
        get-following-rloc (partial rel-trans-dst-dir from-rloc :following 1)
        is-simple? (fn [] (and (= "simple" (:type ch)) (not (:become-queen from-movement))))
        rloc-is-free? (partial rloc-is-free? rel-view)
        get-following-free-rlocs (partial get-following-free-rlocs from-rloc rel-view)
        nearest-chs (
            dfor dst-dir (sorted (.values c-rays) 
                            :key (fn [dst-dir] (:dst dst-dir))
                            :reverse True)
            :if (not (rloc-is-free? (get-rloc (:dst dst-dir) (:dir dst-dir))))
            [(:dir dst-dir) dst-dir]
        )
        has-enemy? (fn [dst dir] (let [rloc (get-rloc dst dir)] (
            and (is-not rloc None)
                (in rloc rel-view) 
                (!= (:team ch) (:team (get rel-view rloc)))))
        )
        enemy-could-be-swallowed? (fn [dst dir] 
            (let [
                enemy-rloc (get-rloc dst dir)
                following-rloc (get-following-rloc dst dir)
            ]
                (and 
                    (not-in enemy-rloc (:swallowed-ch from-movement))
                    (is-not following-rloc None) 
                    (rloc-is-free? following-rloc)
                    (if (is-simple?) (= dst 1) True))
            )
        )
        calculated-movements (lfor pos (.values nearest-chs) 
            :if (and 
                    (has-enemy? #** pos)
                    (enemy-could-be-swallowed? #** pos))
            :setv enemy-rloc (get-rloc (:dst pos) (:dir pos))
            :setv swallowed (swallowed-ch (:swallowed-ch from-movement) enemy-rloc)
            :setv max-dst (if (is-simple?) 1 None)
            :setv enemy-following-free-rlocs (
                get-following-free-rlocs (:dst pos) (:dir pos) :max-dst max-dst
            )

            free-rloc enemy-following-free-rlocs

            :setv r-route (rel-route (:relroute from-movement) free-rloc)

            (movement :relroute r-route :team ch-team :swallowed-ch swallowed)
        )
    ] 
        (flatten (lfor move calculated-movements 
            (or (get-enemies-swallowing-movements ch d-chs 
                :from-movement move)
            move)))
    )
)

(defn get-quiet-movements [ch d-chs / [from-movement None]]
    "Returns quiet movements without any swallowing"
    (let [
        ch-team (:team ch)
        rel-view (checkers-view d-chs ch-team)
        from-movement (or from-movement 
            (movement (rel-route [(:rloc ch)]) ch-team (swallowed-ch []))
        )
        from-rloc (:last-rel-point from-movement)
        is-simple? (= "simple" (:type ch))
        up-dirs [#(-1 1) #(1 1)] 
        down-dirs [#(-1 -1) #(1 -1)]
        max-dst (if is-simple? 0 None)
        selected-dirs (if is-simple? up-dirs (+ up-dirs down-dirs))

        get-rloc (partial rel-trans-dst-dir from-rloc)
        get-following-free-rlocs (partial get-following-free-rlocs from-rloc rel-view)
    ] 
        (lfor dir selected-dirs free-rloc (
            get-following-free-rlocs 1 dir :following 0 :max-dst max-dst)
            :setv r-route (rel-route (:relroute from-movement) free-rloc)

            (movement :relroute r-route :team ch-team :swallowed-ch [])
        )
    )
)

(defn calculate-checker-movements [checker d-chs]
    ; look left-up right-up 1 step if simple
    ; look left-up right-up 1-n steps if queen
    ; if there are is enemy and empty space after it - must eat
    ; look if there are enemies in left-back and right-back and there is space - must eat
    ; if after swallowing we can swallow others - we must to do it
    ; also route must show if simple should become queen
    ; return: list of movements (each contain info about swallowed checkers) or []
    (let [
        swallowing-movements (get-enemies-swallowing-movements checker d-chs)
        quiet-movements (get-quiet-movements checker d-chs)
    ]
        (or swallowing-movements quiet-movements)
    )
)

(defn calculate-all-available-movements [d-chs]
    (let [
        all-movements (flatten 
            (lfor ch (.values d-chs) (calculate-checker-movements ch d-chs)))
        select-by-team (fn [team] (list (filter (fn [m] (= team (:team m))) all-movements)))
        select-by-swallowing (fn [movements has-swallowing] (list 
            (filter (fn [m] (if has-swallowing 
                (:swallowed-ch m) 
                (not (:swallowed-ch m)))) movements)))
        white-movements (select-by-team "white")
        black-movements (select-by-team "black")

        white-swallowing (select-by-swallowing white-movements :has-swallowing True)
        white-quiet (select-by-swallowing white-movements :has-swallowing False)

        black-swallowing (select-by-swallowing black-movements :has-swallowing True)
        black-quiet (select-by-swallowing black-movements :has-swallowing False)
    ]
        {
            "white" (or white-swallowing white-quiet)
            "black" (or black-swallowing black-quiet)
        }
    )
)

(defn calculate-team-scores [d-chs]
    ; queen is like 3 simples
    (let [
        score-by-type {
            "simple" 1
            "queen" 3
        }
        ch-count (fn [team] (
            sum (lfor ch (.values d-chs) 
                    :if (= (:team ch) team) 
                    (get score-by-type (:type ch)))
        ))
        white-count (ch-count "white")
        black-count (ch-count "black")
    ]
        {
            "white" (- white-count black-count)
            "black" (- black-count white-count)
        }
    )
)

(defn who-is-winner? [available-movements]
    (cond 
        (not (:white available-movements)) "black"
        (not (:black available-movements)) "white"
        (and (:white available-movements) (:black available-movements)) None
    )
)

; Rules module tests

(let [d-chs (d-checkers-from-list [
    ; single swallow
    (checker "simple" "white" (abs-loc "d4"))
    (checker "simple" "black" (abs-loc "e5"))

    ; single swallow (second is not possible because of black at e5)
    (checker "simple" "white" (abs-loc "a5"))
    (checker "simple" "black" (abs-loc "b6"))
    (checker "simple" "black" (abs-loc "d6"))

    ; swallow two blacks with choice (f2 f4)
    (checker "queen" "white" (abs-loc "g1"))
    (checker "queen" "black" (abs-loc "d2"))
    (checker "simple" "black" (abs-loc "f2"))
    (checker "simple" "black" (abs-loc "f4"))

    ; white become queen
    (checker "simple" "white" (abs-loc "h6"))
    (checker "simple" "black" (abs-loc "g7"))
])]
    (assert (= 1 (len (get-enemies-swallowing-movements (:d4 d-chs) d-chs))))
    (assert (= 1 (len (get-enemies-swallowing-movements (:a5 d-chs) d-chs))))

    (assert (= [] (get-quiet-movements (:a5 d-chs) d-chs)))
    (assert (= 1 (len (get-quiet-movements (:d4 d-chs) d-chs))))

    ; (print (get-quiet-movements (:d2 d-chs) d-chs))
    ; (print (calculate-checker-movements (:d2 d-chs) d-chs))

    ; (print (:black (calculate-all-available-movements d-chs)))
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "c3"))
    (checker "simple" "black" (abs-loc "d4"))

])
    t1 (get-enemies-swallowing-movements (:c3 d-chs) d-chs)
    t2 (get-enemies-swallowing-movements (:d4 d-chs) d-chs)

    t3 (calculate-checker-movements (:c3 d-chs) d-chs)
    t4 (calculate-checker-movements (:d4 d-chs) d-chs)
]
    (assert (and (= 1 (len t1)) (= ["44"] (:swallowed-ch (get t1 0)))) t1)
    (assert (and (= 1 (len t2)) (= ["66"] (:swallowed-ch (get t2 0)))) t2)

    (assert (and (= 1 (len t3)) (= ["44"] (:swallowed-ch (get t1 0)))) t3)
    (assert (and (= 1 (len t4)) (= ["66"] (:swallowed-ch (get t2 0)))) t4)
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "a1"))
    (checker "simple" "white" (abs-loc "c1"))

    (checker "simple" "white" (abs-loc "f2"))
    (checker "simple" "white" (abs-loc "h2"))
    (checker "simple" "white" (abs-loc "g1"))
])
    t1 (get-quiet-movements (:a1 d-chs) d-chs) 
    t2 (get-quiet-movements (:c1 d-chs) d-chs) 
    t3 (get-quiet-movements (:g1 d-chs) d-chs) 
]
    (assert (= 1 (len t1)) t1)
    (assert (= 2 (len t2)) t2)
    (assert (= 0 (len t3)) t3)
)

(let [d-chs (d-checkers-from-list [
    (checker "queen" "white" (abs-loc "a1"))
    (checker "queen" "white" (abs-loc "d4"))
])
    t1 (get-quiet-movements (:a1 d-chs) d-chs) 
    t2 (get-quiet-movements (:d4 d-chs) d-chs) 
]
    (assert (= 2 (len t1)) t1)
    (assert (= 12 (len t2)) t2)
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "c3"))
    (checker "simple" "black" (abs-loc "e5"))

])
    t1 (get-enemies-swallowing-movements (:c3 d-chs) d-chs)
    t2 (get-enemies-swallowing-movements (:e5 d-chs) d-chs)

    t3 (get-quiet-movements (:c3 d-chs) d-chs) 
    t4 (get-quiet-movements (:e5 d-chs) d-chs)
]
    (assert (= 0 (len t1)) t1)
    (assert (= 0 (len t2)) t2)

    (assert (= 2 (len t3)) t3)
    (assert (= 2 (len t4)) t4)
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "d2"))

    (checker "simple" "black" (abs-loc "e3"))
    (checker "simple" "black" (abs-loc "g3"))
])
    t1 (get-enemies-swallowing-movements (:d2 d-chs) d-chs) 
]
    (assert (= 1 (len t1)) t1)
    (assert (= ["53" "73"] (:swallowed-ch (get t1 0))) t1)
)

(let [d-chs (d-checkers-from-list [
    ; white queen can't eat black because of obstacle
    (checker "queen" "white" (abs-loc "a1"))
    (checker "simple" "white" (abs-loc "b2"))
    (checker "simple" "black" (abs-loc "e5"))
])
    t1 (get-enemies-swallowing-movements (:a1 d-chs) d-chs)
]
    (assert (= 0 (len t1)) t1)
)

(let [d-chs (d-checkers-from-list [
    (checker "queen" "white" (abs-loc "d2"))

    (checker "simple" "black" (abs-loc "f4"))
    (checker "simple" "black" (abs-loc "g7"))
])
    t1 (get-enemies-swallowing-movements (:d2 d-chs) d-chs) 
]
    (assert (= 2 (len t1)) t1)
    (assert (= ["64"] (:swallowed-ch (get t1 0))) t1)
    (assert (= ["64" "77"] (:swallowed-ch (get t1 1))) t1)
)

(let [d-chs (d-checkers-from-list [
    ; simple become queen and eats two checkers
    (checker "simple" "white" (abs-loc "h6"))

    (checker "simple" "black" (abs-loc "g7"))
    (checker "simple" "black" (abs-loc "b4"))
])
    t1 (get-enemies-swallowing-movements (:h6 d-chs) d-chs) 
]
    (assert (= 1 (len t1)) t1)
    (assert (= ["77" "24"] (:swallowed-ch (get t1 0))) t1)
    (assert (:become-queen (get t1 0)) t1)
)

(let [d-chs (d-checkers-from-list [
    (checker "queen" "white" (abs-loc "a1"))
    (checker "queen" "white" (abs-loc "b2"))
    (checker "queen" "white" (abs-loc "c1"))
    (checker "queen" "white" (abs-loc "d2"))
])
    t1 (get-quiet-movements (:a1 d-chs) d-chs) 
    t2 (get-quiet-movements (:c1 d-chs) d-chs) 

    t3 (calculate-checker-movements (:a1 d-chs) d-chs)
    t4 (calculate-checker-movements (:c1 d-chs) d-chs)
]
    (assert (= 0 (len t1)) t1)
    (assert (= 0 (len t2)) t2)

    (assert (= t1 t3))
    (assert (= t2 t4))
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "c3"))
    (checker "simple" "black" (abs-loc "e5"))

])
    t1 (who-is-winner? (calculate-all-available-movements d-chs))
]
    (assert (is t1 None) t1)
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "white" (abs-loc "c3"))
])
    t1 (who-is-winner? (calculate-all-available-movements d-chs))
]
    (assert (= t1 "white") t1)
)

(let [d-chs (d-checkers-from-list [
    (checker "simple" "black" (abs-loc "c3"))
])
    t1 (who-is-winner? (calculate-all-available-movements d-chs))
]
    (assert (= t1 "black") t1)
)

(let [d-chs (d-checkers-from-list [
    ; no place to move for white
    (checker "simple" "white" (abs-loc "h2"))
    (checker "simple" "black" (abs-loc "g3"))
    (checker "simple" "black" (abs-loc "f4"))
])
    t1 (who-is-winner? (calculate-all-available-movements d-chs))
]
    (assert (= t1 "black") t1)
)