(ns samba.grid
  (:require [samba.patterns :as pat]
            [samba.sequencer :as sequencer]
            [reagent.core :as reagent])
  )

(defn drum-line [{key :key
                  extended-from :extended-from} pattern
                 is-highlight]
  [:div.drum-line
   {:key key}
   (let [by-beat (group-by :beat pattern)
         beats (sort (keys by-beat))]
     (for [beat beats]

       [:span.beat {:key beat
                    :class (when (and extended-from (> beat extended-from)) "extended")
                    }
        (let [notes (by-beat beat)]
          (for [{type :type note :note time :time} notes]
            [:span.note {:key note
                         :style {:color (when (is-highlight {:beat beat :note note :time time})
                                          :red)
                                 :border (when (= type :accent) "1px #ccc solid")
                                 }
                         }
             (case type :accent "•" :sound "•" :rest "-")]))]))])

(defn drum-lines
  [{instruments :instruments} patterns]
  (let [inner-state (reagent/atom
                     {:playing false
                      :selected-pattern
                      (into {}
                            (for [[k v] patterns]
                              [k (first (first v))]))})
        selected-pattern
        (reagent/cursor inner-state [:selected-pattern])

        playing
        (reagent/cursor inner-state [:playing])

        ;; we need a sequencer which we will turn on and off
        sq (sequencer/create-sequencer)
        ]
    ;; I can add a watch on inner-state here
    ;; but I want to destroy that watch when
    ;; the component goes away...
    ;; or I can control sq and state directly
    (sequencer/set-patterns!
     sq
     (into {}
           (for [[k p] @selected-pattern]
             [k (pat/complete-pattern (get-in patterns [k p]))])))

    (sequencer/set-fx! sq instruments)

    (fn [{extend-to :extend-to} patterns]
      (into [:div
             [:button {:on-click
                       (fn []
                         (let [playing (swap! playing not)]
                           (sequencer/set-playing! sq playing
                                                   (fn []))))

                       } (if (:playing @inner-state) "Pause" "Play")]
             ]
            (for [[k ps] patterns]
              (let [pattern-name (@selected-pattern k)
                    pattern (ps pattern-name)
                    pattern (pat/complete-pattern pattern)
                    ]
                [:div.pattern-container
                 {:key k}
                 ;; TODO switches for all the keys
                 [:div.pattern-header [:b k] " (" pattern-name ")"]
                 [drum-line {:extended-from
                             (apply max (map :beat pattern))
                             }
                  (pat/extend pattern extend-to)
                  (constantly false)]])))))

  )
