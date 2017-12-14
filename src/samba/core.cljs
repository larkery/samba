(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [clojure.string :as string]
            [samba.grid :as grid]
            [samba.sequencer :as sq]
            [samba.patch :as patch]
            [samba.instruments :as instruments]
            ))

(enable-console-print!)

(defn on-js-reload [])

(def surdos #{:s1 :s2 :s3 :s4})
(def drums #{:s1 :s2 :s3 :s4 :cai :rep})

(let [labels-map {:s1 "1st"
                  :s2 "2nd"
                  :s3 "3rd"
                  :s4 "4th"
                  :cai "Caixa"
                  :rep "Repinique"
                  :agg "Agogo"
                  :mes "Mestre"

                  drums "All drums"
                  surdos "Surdos"
                  }]
  (defn instrument-labels [k]
    (or (labels-map k)
        (and (keyword? k) (name k))
        (string/join ", " (map instrument-labels k)))))

(def instrs
  {:s1  instruments/surdo-1
   :s2  instruments/surdo-2
   :s3  instruments/surdo-3
   :s4  instruments/surdo-4
   :cai instruments/snare
   :rep instruments/repi2
   :mes instruments/repi2
   :agg instruments/agogo
   })

(let [accent (atom false)]
  (defn sound-button [sound]
    [:button {:role :button
              :on-click #(sound (patch/now) (swap! accent not))}
     "🔉"]))

(let [mutes
      (reagent/atom {})

      play-instrument
      (fn play-instrument [i t a]
        (if-let [instr (instrs i)]
          (let [mute (@mutes i)]
            (case mute
              :accent (if a (instr t a))
              :mute nil
              (instr t a)))
          (if (seq i)
            (doseq [i i] (play-instrument i t a)))))

      sequencer
      (sq/create-sequencer
       {:fx play-instrument :tempo 100}
       reagent/atom)

      tempo (reagent/cursor sequencer [:tempo])

      dl
      (fn [patterns breaks]
        [grid/drum-lines
         {:sequencer sequencer :extend-to 4
          :labels instrument-labels}
         patterns breaks])


      sequencer-controls
      (fn []
        ;; show nothing if not playing.

        [:div {:style {:display :flex :flex-wrap :wrap :flex-direction :row :padding-bottom :0.2em
                       :align-items :center}}
         [:span
          (doall (for [i (keys instrs)]
                   [:button.muter
                    {:key i
                     :class (str (name (or (@mutes i) :normal))
                                 " cue"
                                 )
                     :on-click
                     #(swap! mutes update i
                             {:mute nil, nil :accent, :accent :mute})}

                    (instrument-labels i) " "

                    (case (@mutes i)
                      :accent [:strong "!"]
                      :mute "🔇"
                      "🔉")]))]

         (when (:is-playing @sequencer)
           [:div {:style {:display :flex :flex-direction :column :margin-left :auto :align-items :center}}
            [:input {:type :range :style {:width :7em}
                     :min 70 :max 130 :step 5
                     :value @tempo
                     :on-change #(let [value (.. % -target -value)]
                                   (reset! tempo value))
                     }]
            [:small @tempo "bpm"]
            ]
           )

         (when (:is-playing @sequencer)
           [:button.cue {:on-click #(sq/stop! sequencer)} "Stop"]
           )

         ])

      ]

  (def blanks
    (doall
     {:sequencer [sequencer-controls]
      :caixa-sound [sound-button instruments/snare]
      :repi-sound [sound-button instruments/repi2]
      :s1-sound [sound-button instruments/surdo-1]
      :s2-sound [sound-button instruments/surdo-2]
      :s3-sound [sound-button instruments/surdo-3]
      :s4-sound [sound-button instruments/surdo-4]
      :agogo-high [sound-button instruments/bell-high]
      :agogo-low [sound-button instruments/bell-low]

      :example-pattern
      [dl
       {:s1 {:1 '[! _ _ _ . _ _ _]
             :2 '[. _ ! _ . ! _ !]}}
       ]

      :reggae-patterns
      [dl
       ;; patterns
       {:s1  {:1 '[_ _ _ _ | . _ _ _ ]}
        :s2  {:1 '[. _ _ _ | _ _ _ _ ]}
        :s3  {:1 '[. _ _ _ | _ _ _ _ | . . . . | . . . . |
                   . _ _ _ | _ _ _ _ | _ _ _ _ | . . . .]
              }
        :s4  {:1  '[. _ _ _ | . _ ! _ | . _ _ _ | . ! _ !]
              :2  '[_ _ _ _ | _ . . _ ]
              }
        :rep {:1 '[! . . ! | . . ! . | . . ! . | . ! . .]
              :3 '[! . . ! | . . ! . | . . ! . | ! . . .]
              :5 '[! . . ! | . . . ! | . . ! . | ! . . .]
              }
        :cai {:1 '[. . ! .]}
        }

       ;; breaks
       {:1
        [{:mes '[24
                 . _ _ _;4
                 . _ _ _;8
                 . _ _ _;12
                 . _ _ _;16
                 _ _ . _
                 _ _ _ _

                 . _ _ _
                 _ _ . _
                 _ _ . _
                 _ _ . _
                 _ _ . _
                 _ _ _ _

                 4 . .

                 ]
          }


         {drums '[ . . . . ]}
         {:mes '[24
                 . _ _ _;4
                 . _ _ _;8
                 . _ _ _;12
                 . _ _ _;16
                 _ _ . _
                 _ _ _ _

                 . _ _ _
                 _ _ . _
                 _ _ . _
                 _ _ . _
                 _ _ . _
                 _ _ _ _

                 4 . .

                 ]
          }


         {drums '[ . . . . ]}
         ]
        :2
        [{drums
          '[. . _ . | _ . _ . | _ . _ . | _ . _ . ]
          }]

        "'Horizontal Sticks'"
        [{surdos
          '[! | _ | _ | _ |
            ! | _ | _ | _ |

            ;; ! | ! | ! | ! . ! _
            ! _ _ ! | _ _ ! _ | _ _ ! _ | ! _ ! !
            | | | |

            3 . . . . . . . . . . . .
            ]
          :cai :continue
          }

         ]

        :4
        [{#{:s1 :s2 :s3 :s4 :rep}
          '[. . _ _ | _ _ . _ | . . _ _ | _ |
            . . _ _ | . _ . _ | . . _ _ | _ |
            . . _ _ | _ _ . _ | . . _ _ | _ |
            . . _ _ | . _ . _ | . _ . _ ]
          :cai :continue
          }
         {:s3 '[. . . .]}

         ]
        }


       ]

      :funk-patterns
      [dl
       {:s1 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
        :s2 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
        :s3 {:1 '[_ _ _ _ | _ _ _ _ | _ _ _ . | ! _ . _]}
        :s4 {:1 '[. . . . | . _ _ _ | _ _ _ _ | _ _ _ _]}
        :rep {:1 '[_ _ _ _ | . _ . _ | . _ _ _ | . _ _ _]} ;; TODO maybe wrong
        :cai {:1 '[. . ! .]}}]

      :six-eight-patterns
      [dl
       {:s1 {:1 '[3 _ _ _ | . _ _]}
        :s2 {:1 '[3 . _ _ | _ _ _]}
                                        ;      :s3 {}
        :s4 {:1 '[3 . . . | _ _ _ | _ _ _ | _ _ _]}
                                        ;      :rep {} ;; the pattern I can't remember
        :cai {:1 '[3 . . .]}
        }
       ]

      :aforche-patterns
      [dl
       {:s3 {:1 '[_ _ . _]
             :2 '[_ . . _]
             }
        :cai {:1 '[. . ! .]}
        :rep {:1 '[! _ _ _]}
        :agg {:1 '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]}
        } ; TODO accents vs hands

       {"Break 1 (repeat x4)"
        [{:agg '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]}
         {:agg '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]
          drums '[_ | _ | ! _ ! _ | ! _ ! _ ]}
         ]
        :2
        [{drums '[. _ . . | _ . . _ | . _ . . | _ . . _ | . _ . . | _ . . _ | ! _ _ _ | _ |
                  . _ . . | _ . . _ | . _ . . | _ . . _ | . _ . . | _ . . _ | ! _ !  _ | ! _ ! _
                  ]}]
        }

       ]
      })


    ))

(doseq [[k v] blanks]
  (when-let [element (js/document.getElementById (name k))]
    (reagent/render-component v element)))
