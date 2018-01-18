(ns samba.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [samba.grid :as grid]
            [samba.sequencer :as sq]
            [samba.patch :as patch]
            [samba.instruments :as instruments]
            [samba.patterns :as patterns]
            [samba.patterns.samba-reggae :as samba-reggae]
            [samba.patterns.samba-funk :as samba-funk]
            [samba.patterns.six-eight :as six-eight]
            [samba.patterns.aforxe :as aforxe]
            [samba.patterns.baiao :as baiao]
            ))

(enable-console-print!)

(defn on-js-reload [])

(let [labels-map {:s1 "1st"
                  :s2 "2nd"
                  :s3 "3rd"
                  :s4 "4th"
                  :cai "Caixa"
                  :rep "Repinique"
                  :agg "Agogo"
                  :mes "Mestre"

                  patterns/drums "All drums"
                  patterns/surdos "Surdos"
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
       {:s1 {:1 '[# _ _ _ . _ _ _]
             :2 '[. _ # _ . # _ #]}}
       ]

      :reggae-patterns
      [dl samba-reggae/patterns samba-reggae/breaks]

      :funk-patterns
      [dl samba-funk/patterns samba-funk/breaks]

      :six-eight-patterns
      [dl six-eight/patterns]


      :aforche-patterns
      [dl aforxe/patterns aforxe/breaks]

      ;; missing instruments:
      ;; triangle, shaker
      ;; shaker is basically shaped noise with a bandpass
      ;; triangle sounds ring modulated?
      :baiao
      [dl baiao/patterns baiao/breaks]

      })


    ))

(doseq [[k v] blanks]
  (when-let [element (js/document.getElementById (name k))]
    (reagent/render-component v element)))
