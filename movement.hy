(import utils [pairwise])
(import location [to-abs-loc dir-between-rloc rloc-in-up-line?])

(defn abs-route [absroute #* to-add]
    (+ absroute (list to-add))
)

(defn abs-route-from-rel-route [relroute team #* to-add]
    (lfor rloc (+ relroute (list to-add)) (to-abs-loc rloc team))
)

(defn rel-route [relroute #* to-add]
    (+ relroute (list to-add))
)

(defn swallowed-ch [swallowed-ch #* to-add]
    (+ swallowed-ch (list to-add))
)

(defn movement [relroute team swallowed-ch]
    (let [
        at-least-one-rel-point? (and (is-not relroute None) (>= (len relroute) 1))
        has-rel-route? (and (is-not relroute None) (> (len relroute) 1))
    ]
        {
            "team" team
            "relroute" relroute
            "swallowed_ch" swallowed-ch
            "become_queen" (any (map rloc-in-up-line? relroute))
            "last_dir" (if has-rel-route? 
                            (dir-between-rloc (get relroute -2) (get relroute -1)
                        ) None)
            "first_rel_point" (if at-least-one-rel-point? (get relroute 0) None)
            "last_rel_point" (if at-least-one-rel-point? (get relroute -1) None)
        }
    )
)


(assert (= ["a1" "c3" "e1"] (abs-route-from-rel-route ["11" "33" "51"] "white")))
(assert (= ["a1" "c3" "e1" "g3"] (abs-route-from-rel-route ["11" "33" "51"] "white" "73")))