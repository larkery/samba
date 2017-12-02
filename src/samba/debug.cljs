(ns samba.debug)
(defn structure [thing]
  (cond
    (map? thing)
    [:div
     (for [[k v] thing]
       [:div {:style {:margin-left :1em}
              :key k}
        [:b (str k)]
        [structure v]])]

    (or (seq? thing)
        (vector? thing)
        )
    [structure
     (into {} (map-indexed vector thing))
     ]



    :otherwise
    [:div (str thing)]


    )
  )
