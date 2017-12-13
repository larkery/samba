(ns samba.grid
  (:require [samba.patterns :as pat]
            [samba.sequencer :as sequencer]
            [reagent.core :as reagent]
            )
  )

(defn switches
  [{sel :selection
    opts :options
    on-change :on-change}]

  [:span.switches
   (for [[k v] opts]
     (if (= k sel)
       [:span.switch.selected {:key k} v]
       [:span.switch
        {:key k
         :on-click #(on-change k)
         :style {:cursor :pointer}
         :role :button}
        v]))])


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
                         :style {:color
                                 (cond
                                   (is-highlight {:beat beat :note note :time time})
                                   :red
                                   (= type :rest)
                                   :grey
                                   )

                                 }
                         }
             (case type :accent "!" :sound "•" :rest "-")]))]))])

(defn drum-lines
  "Draw a set of drum lines, and also wire up a synthesizer to play them.
  PATTERNS contains the normal pattern set for the instruments, and BREAKS the breaks.
  In PATTERNS we map instrument name to a map from pattern name to pattern.
  In BREAKS we map break name to something more complicated.
  "

  [{instruments :instruments} patterns breaks]
  (let [inner-state (reagent/atom
                     {:playing false
                      :tempo 100
                      :play-mode
                      (into {}
                            (for [k (keys patterns)]
                              [k :play]))
                      :selected-pattern
                      (into {}
                            (for [[k v] patterns]
                              [k (first (first v))]))})

        play-mode
        (reagent/cursor inner-state [:play-mode])

        selected-pattern
        (reagent/cursor inner-state [:selected-pattern])

        playing
        (reagent/cursor inner-state [:playing])

        tempo
        (reagent/cursor inner-state [:tempo])

        sq (sequencer/create-sequencer)

        set-patterns!
        #(sequencer/set-patterns!
          sq
          (let [play-mode @play-mode]
            (into {}
                  (for [[k p] @selected-pattern]
                    [k (pat/complete-pattern
                        (case (play-mode k)
                          :play (get-in patterns [k p])
                          :mute '[_]
                          :accents (replace '{. _} (get-in patterns [k p]))
                          ))]))))

        set-tempo!
        #(sequencer/set-tempo! sq @tempo)
        ]

    (set-patterns!)
    (sequencer/set-fx! sq instruments)
    (set-tempo!)


    (fn [{extend-to :extend-to} patterns breaks]
      [:div
       [:div.controls
        [:span "Patterns"]
        [:label.bpm
         [:small (str @tempo "bpm ")]
         [:input {:type :range
                  :style {:width :8em}
                  :min 70 :max 130
                  :step 5
                  :value @tempo
                  :on-change #(let [value (.. % -target -value)]
                                (reset! tempo value)
                                (set-tempo!))
                  }]
         ]

        [:button.play {:on-click
                  (fn []
                    (let [playing (swap! playing not)]
                      (sequencer/set-playing! sq playing (fn []))))}
         (if (:playing @inner-state) "Stop" "cue!")]]

       (doall
        (for [[k ps] patterns]
          (let [pattern-name (@selected-pattern k)
                cur-play-mode (@play-mode k)
                other-patterns (keys ps)
                pattern (ps pattern-name)
                pattern (pat/complete-pattern pattern)
                ]
            [:div.pattern-container
             {:key k}
             [:div.pattern-header
              [:b k]
              [:span
               {:on-click
                #(do (swap! play-mode update k {:play :mute :mute :accents :accents :play})
                     (set-patterns!))
                :style {:width :1em :margin-left :0.5em
                        :text-align :center
                        :cursor :pointer}
                }
               ({:mute "🔇" :accents [:strong "!"] :play "🔉"} cur-play-mode)
               ]
              ]

             [drum-line {:extended-from (apply max (map :beat pattern))}
              (pat/extend pattern extend-to)
              (constantly false)]

             [switches {:selection pattern-name
                        :options (for [p2 other-patterns] [p2 (name p2)])
                        :on-change #(do (swap! selected-pattern assoc k %)
                                        (set-patterns!))
                        }]
             ])))
       (doall
        (for [[break units] breaks]
          [:div.break {:key break}
           [:div.title
            [:span.name (str "Break " (name break))]
            [:button "cue!"]
            ]
           (for [[i unit] (map-indexed vector units)]
             [:div.break-unit {:key i}
              (for [[k pat] unit]
                [:div.pattern-container {:key k}
                 [:div.pattern-header [:b (str k)]]

                 [drum-line {}
                  (pat/complete-pattern pat) (constantly false)
                  ]
                 ])]
             )
           ]
          ))

       ]))

  )
