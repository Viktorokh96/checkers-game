(import utils [assoc dissoc as-is])
(import location [to-rel-loc to-abs-loc])


(defn checker [type team aloc]
    (assert (in team ["white" "black"]))
    (assert (in type ["simple" "queen"]))
    {
        "meta" {"obj-type" "checker"} 
        "type" type 
        "team" team 
        "aloc" aloc
        "rloc" (to-rel-loc aloc team)
    }
)

(defn checker-updated [ch #** changes]
    (checker 
        :type (:type changes (:type ch))
        :team (:team changes (:team ch))
        :aloc (:aloc changes (:aloc ch))
    )
)

(defn d-checkers [d-chs / [modifier as-is]]
    (modifier d-chs)
)

(defn d-checkers-from-list [l-ch / [modifier as-is]]
    (d-checkers (dfor c l-ch [(:aloc c) c])
        :modifier modifier
    )
)

(defn checker-movement-applied [mv d-chs]
    (let [
        loc-from (to-abs-loc (:first-rel-point mv) (:team mv))
        loc-to (to-abs-loc (:last-rel-point mv) (:team mv))
        checker (get d-chs loc-from)
        swallowed (lfor sw (:swallowed-ch mv) (to-abs-loc sw (:team mv)))
        become-queen? (:become-queen mv)
        ch-type (if become-queen? "queen" (:type checker))
    ]
        (d-checkers
            (assoc 
                (dissoc d-chs loc-from #* swallowed) 
                #** {loc-to (checker-updated checker :aloc loc-to :type ch-type)}
            )
        )
    )
)

(defn checkers-view [d-chs team]
    (assert (in team ["black" "white"]))
    (dfor [aloc c] (.items d-chs) [(to-rel-loc aloc team) c])
)
