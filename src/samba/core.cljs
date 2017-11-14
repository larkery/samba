(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [clojure.string :as string]
            [samba.sequencer :as sequencer]
            [samba.sound :as sound]
            [cljsjs.material-ui]
            [cljs-react-material-ui.core :as ui-core]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defn classes [& cs] (string/join " " (filter identity cs)))

(def initial-state
  {:playing false
   :instruments
   (let [;; turns the patterns map the other way around
         instrument-patterns
         (reduce
          (fn [acc [instrument-name pattern-name]]
            (let [pattern (or (get-in pat/patterns [pattern-name instrument-name])
                              (get-in pat/patterns [pattern-name :ALL]))
                  ]
              (if (seq pattern)
                (assoc-in acc [instrument-name pattern-name]
                          (pat/complete-pattern pattern))
                acc)))
          {}
          (for [instrument pat/instruments
                pattern-name (keys pat/patterns)]
            [instrument pattern-name]))
         ]
     (into {}
           (for [instrument pat/instruments]
             [instrument
              {:name (pat/names instrument)
               :mute false
               :pattern (get-in instrument-patterns [instrument :R1])
               :patterns (instrument-patterns instrument)}]
             )))}
  )

(defonce state (reagent/atom initial-state))
(defonce live-notes (reagent/atom #{}))
(defonce machine (sequencer/create-sequencer))

(sequencer/set-fx!
 machine
 {:S1 (sound/beep :freq :C2 :length 2.5)
  :S2 (sound/beep :freq :F2 :length 2.5)
  :S3 (sound/beep :freq :B3 :length 1.5 :shape :triangle)
  :S4 (sound/beep :freq :A3 :length 2 :shape :triangle)
  :SN (sound/vol (sound/sample "/fx/snare-vinyl02.wav") 0.1)
  :HP (sound/vol (sound/sample "/fx/snare-dist02.wav") 0.1)
  :M  (sound/squares)
  }
 )

(defn update-sequencer []
  (let [{instruments :instruments
         playing :playing} @state]
    (println "Updating sequencer!")
    (sequencer/set-patterns!
     machine
     (into {} (for [[i {pat :pattern}] instruments] [i pat])))
    (sequencer/set-playing!
     machine playing
     (fn [live]
       (reset! live-notes
               (into #{}
                     (map select-keys
                          live
                          (repeat [:beat :note :time]))))
       ))))

(defonce tracker (reagent/track! update-sequencer))

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


(defn instrument-block [{key :key extend-to :extend-to live-notes :live-notes} instrument-state]
  ;; TODO maybe repeat drum line with ghostly patterns for the repeat blocks
  (let [expanded (reagent/atom false)]
    (fn [{key :key extend-to :extend-to live-notes :live-notes} instrument-state]
      (let [{pattern :pattern
             patterns :patterns
             instrument-name :name} @instrument-state

            current-pattern-name
            (or (->> patterns
                     (filter (fn [[n p]] (= p pattern)))
                     (first)
                     (first))
                "??")
            ]
        [ui/paper
         {:key key}
         [:div.pattern-container

          [:div.pattern-header {:on-click #(swap! expanded not)
                                :role :button}
           [:b instrument-name] " " [:em current-pattern-name]
           ]

          [drum-line {:extended-from (apply max (map :beat pattern))}
           (pat/extend pattern extend-to)
           live-notes
           ]]

         (when @expanded
           [:div {:style {:border-top "1px grey solid"
                          :border-bottom "1px grey solid"}}
            (for [[pattern-name pat] patterns
                  :when (not (= pattern-name current-pattern-name))]
              [:div.pattern-container {:key pattern-name
                                       :style {:background :#fef}}
               [:div.pattern-header
                [:button {:on-click #(do
                                       (swap! expanded not)
                                       ;; TODO make the cue button actually cue up rather than swapping now
                                       (swap! instrument-state assoc :pattern pat))} "Cue!"]
                [:em pattern-name]]

               [drum-line {:key pattern-name} pat (constantly false)]]
              )
            ])
         ])))
  )

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

(defn main-component []
  [ui/paper
   [ui/toolbar
    [ui/toolbar-group "Instruments"]
    [ui/toolbar-group
     [ui/raised-button {:label (if (:playing @state) "Stop" "Play")
                        :on-click #(swap! state update :playing not)}]]]

   (let [max-beats
         (for [[_ {pat :pattern}] (:instruments @state)]
           (apply max (map :beat pat)))

         max-beats (filter #(< 0 %) max-beats)

         loop-length (if (empty? max-beats)
                       4
                       (reduce pat/lcm max-beats))

         live-notes (into #{}
                          (map (fn [m] (update m :beat #(+ 1 (mod % loop-length))))
                               @live-notes))
         ]

     (for [instrument pat/instruments]
       [instrument-block {:key instrument :extend-to loop-length
                          :live-notes live-notes
                          }
        (reagent/cursor state [:instruments instrument])])
     )

   ;; buttons to cue particular breaks for everyone playing the break
   ;; something to make the cueing work
   (for [key (keys pat/patterns)]
     [ui/flat-button
      {:key key
       :on-click
       #(swap! state
               (fn [state]
                 (reduce
                  (fn [state i]
                    (let [pk (or (get-in pat/patterns [key i])
                                 (get-in pat/patterns [key :ALL])
                                 '[_])]
                      (assoc-in state [:instruments i :pattern] (pat/complete-pattern pk))
                      ))
                  state
                  pat/instruments))
               )
       }
      key
      ])

   [structure @state]

   ]
  )

(reagent/render-component
 [ui/mui-theme-provider
  {:mui-theme (ui-core/get-mui-theme)}
  [main-component]]


 (. js/document (getElementById "app")))
