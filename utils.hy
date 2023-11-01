(import itertools [tee])

(defn as-is [arg]
    arg
)

(defn assoc [d #** key-values]
    (| d key-values)
)

(defn dissoc [d #* keys-to-delete]
    (dfor [k v] (.items d) :if (not-in k keys-to-delete) [k v])
)

(defn pairwise [iterable]
    (setv [a b] (tee iterable))
    (next b None)
    (zip a b)
)

(defn first [collection] (next (iter collection) None))

(defn flatten-gen [items]
    (for [i items] 
        (if (isinstance i #(list tuple))
            (for [sub-x (flatten-gen i)]
                (yield sub-x)
            )
            (yield i)
        )
    )
)

(defn flatten [items]
    (list (flatten-gen items))
)

(assert (= [1 2 3 1 "a" {"k" 1}] (flatten [1 2 [3 [1] "a" [{"k" 1}]]])))