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

      ;; records the time when things are going to flash
      flash-queue (atom {})
      ;; updated in request animation frame to change what is flashing
      flashes (reagent/atom {})

      play-instrument
      (fn play-instrument [i t a]
        (if-let [instr (instrs i)]
          (let [p-now (/ (js/performance.now) 1000)
                a-now (patch/now)

                mute (@mutes i)
                play (case mute :accent a :mute false true)
                ]
            (when play
              (instr t a)
              (swap! flash-queue update i conj (- (+ p-now t) a-now))))
          (if (seq i)
            (doseq [i i] (play-instrument i t a)))))

      _ (js/window.requestAnimationFrame
         (fn animate [time]
           (let [time (/ time 1000)]
             (swap! flash-queue
                    (fn [flash-queue]
                      (->> flash-queue
                           (map (fn [[k vs]] [k (filter (partial < (- time 0.1)) vs)]))
                           (into {}))))

             (reset! flashes
                     (->> @flash-queue
                          (map (fn [[k vs]] [k (some (partial > time) vs)]))
                          (into {})))

             (js/window.requestAnimationFrame animate)
             )))


      sequencer
      (sq/create-sequencer
       {:fx play-instrument :tempo 80}
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
        (if (:is-playing @sequencer)
          [:div.sequencer {:style {:display :flex :flex-wrap :wrap :flex-direction :row :padding-bottom :0.2em
                                   :align-items :center}}

           [:div {:style {:display :flex :flex-direction :column :margin-right :auto :align-items :center}}
            [:input {:type :range :style {:width :7em}
                     :min 50 :max 120 :step 5
                     :value @tempo
                     :on-change #(let [value (.. % -target -value)]
                                   (reset! tempo value))
                     }]
            [:small @tempo "bpm"]

            ]

           [:span.mutes
            (doall (for [i (keys instrs)]
                     [:button.muter
                      {:key i
                       :class (str (name (or (@mutes i) :normal))
                                   " cue"
                                   (when (@flashes i) " flash"))
                       :on-click
                       #(swap! mutes update i
                               {:mute nil, nil :accent, :accent :mute})}

                      [:span.light]

                      (instrument-labels i) " "
                      ]))]


           [:button.cue {:on-click #(sq/stop! sequencer)} "Stop"]
           ]
          [:span]))

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
              ;;              :3 '[! . . ! | . . ! . | . . ! . | ! . . .]  this is son clave which we do not use here
              :3 '[! . . ! | . . ! . | ! . . ! | . . ! .]
              :5 '[! . . ! | . . . ! | . . ! . | ! . . .]
              }
        :cai {:1 '[. . ! .]}
        }

       ;; breaks
       {:1
        [
         {:mes '[6 . . . 4 . . | . _ 6 . . . 4 . . . _ ]}
         {drums '[ . . . . | _ |]}
         {:mes '[6 . . . 4 . . | . _ 6 . . . 4 . . . _ ]}
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

            ! _ _ ! | _ _ ! _ | _ _ ! _ | ! _ ! !

            | | | | |

            3 . . . . . . . . . . . .
            ]
          :rep :continue
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
        :rep {:1? '[_ _ _ _ | !  _ . . | _ _ _ _ | ! _ _ _]} ;; TODO maybe wrong
        :cai {:1 '[. . ! .]}}

       {:1
        [
         {:rep    '[6 . . . _ _ . | . . _ _ . . | 4 ! _ ! _ ! _ ! !]
          surdos  '[_ | _ |   ! _ ! _ ! _ _ _]
          }
         ;;  }

         ]
        }
       ]

      :six-eight-patterns
      [dl
       {:s1 {:1 '[3 _ _ _ | . _ _]}
        :s2 {:1 '[3 . _ _ | _ _ _]}
                                        ;      :s3 {}
        :s4 {:1 '[3 . . . | _ _ _ | _ _ _ | _ _ _]}
        :rep {:1 '[3 ! . ! | . ! ! | . ! . | ! ! . ]
              }
        :cai {:1 '[3 . . .]}
        }
       ]

      :aforche-patterns
      [dl
       {surdos {:1 '[h h . _]
                :2 '[h . . _]
                :5 '[h h . _ | h h . _ | h h . h | . h . h]
                }
        :cai {:1 '[. . ! .]}
        :rep {:1 '[! _ _ _]
              :2 '[! ! _ _ | _ _ _ _ | ! _ ! _ | _ _ _ _
                   ! ! _ _ | _ _ _ _ | 6 ! . . 4 ! _ | 6 ! . . 4 ! _
                   ! ! _ _ | _ _ _ _ | ! _ ! _ | _ _ _ _
                   ! ! _ _ | _ _ _ _ | 3 ! ! ! | 4 ! _ _ _

                   ! ! _ _ | _ _ _ _ | ! _ ! _ | _ _ _ _
                   ! ! _ _ | _ _ _ _ | 6 ! . . 4 ! _ | 6 ! . . 4 ! _
                   ! ! _ _ | _ _ _ _ | ! _ ! _ | _ _ _ _
                   ! ! _ _ | _ _ _ _ | ! _ ! _ | ! _ ! _
                   ]
              }
        :agg {:1 '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]}
        }

       {"Break 1 (repeat x4)"
        [{:agg '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]}
         {:agg '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]
          drums '[_ | _ | ! _ ! _ | ! _ ! _ ]}
         ]
        :2 [{drums '[. _ _ . | _ _ . _ | . _ _ . | _ _ . _ ]
             :agg :continue
             }]
        :3
        [{drums '[. _ . . | _ . . _ | . _ . . | _ . . _ | . _ . . | _ . . _ | ! _ _ _ | _ |
                  . _ . . | _ . . _ | . _ . . | _ . . _ | . _ . . | _ . . _ | ! _ !  _ | ! _ ! _
                  ]
          :agg :continue}]
        }

       ]

      ;; missing instruments:
      ;; triangle, shaker
      ;; shaker is basically shaped noise with a bandpass
      ;; triangle sounds ring modulated?
      :baiao
      [dl
       {:agg {:1 '[! _ _ _ | . _ _ _]
              :2 '[_       | . _ . _ | _ ! _ ! | . _ . _]
              }

        :cai {:1 '[! . . ! | . . ! . | . ! ! . | ! . ! .]}

        :rep {:1 '[. . . . | ! . ! . | . ! . . | ! . ! . ]
              :2 '[. . . . | ! . ! . | . ! . . | ! . ! ! ]}

        :s4 {:1 '[h h . _]
             :2 '[. _ _ _ | . _ ! _ | . _ _ _ | . _ ! !]
             }

        #{:s1 :s2 :s3}
        {:1 '[. _ _ . | _ _ _ _ |  . . _ . | _ _ _ _]
         :2 '[. _ _ _ | . _ _ _ | _ | _ ]}

        ;;:tri {:1 '[. . ! .]} ;; I think there's an accent on the off-beat?

        }
       {"Break 1 (first time)"
        [{:agg :continue
          :cai :continue
          :rep :continue
          surdos
          '[! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | ! ! _ ! | _ ! ! _
            ]
          }]

        "Break 1 (second time)"
        [{:agg :continue
          :cai :continue
          (conj surdos :rep)
          '[! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | _ _ _ _ | _ _ _ _ |
            ! _ _ _ | ! _ _ _ | ! ! _ ! | _ ! ! _
            ]
          }]

        "Sock puppet"
        [{:agg '[! _ _ _ | . _ _ _ | ! _ _ _ | . _ _ _ ]}
         {:agg '[! _ _ _ | . _ _ _ | ! _ _ _ | . _ _ _ ] :rep :continue}
         {:agg '[! _ _ _ | . _ _ _ | ! _ _ _ | . _ _ _ ] :rep :continue :cai :continue}
         {:agg '[! _ _ _ | . _ _ _ | ! _ _ _ | . _ _ _ ] :rep :continue :cai :continue :s4 :continue}
         ]

        }
        ]
      })


    ))

(doseq [[k v] blanks]
  (when-let [element (js/document.getElementById (name k))]
    (reagent/render-component v element)))
