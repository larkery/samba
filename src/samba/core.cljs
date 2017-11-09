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
 {:S1 (sound/beep :freq :A2 :length 2.5)
  :S2 (sound/beep :freq :F2 :length 2.5)
  ;;:S3 (sound/beep :freq :A3 :length 1 :shape :triangle)
  ;;:S4 (sound/fuzz (sound/beep :freq :F3 :length 1))
  ;;:SN (sound/squares)
  :HP (sound/beep :freq :A4 :length 1)
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
                         :style (when (is-highlight {:beat beat :note note :time time})
                                  {:color :red})
                         }
             (case type :accent "⬤" :sound "⭘" :rest "-")]))]))])


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
                [:button {:on-click #(swap! instrument-state assoc :pattern pat)} "Cue!"]
                [:em pattern-name]]

               [drum-line {:key pattern-name} pat (constantly false)]]
              )
            ])
         ])))
  )

(defn main-component []
  [ui/paper
   [ui/toolbar
    [ui/toolbar-group "Tom's Samba Drumkit"]
    [ui/toolbar-group
     [ui/raised-button {:label (if (:playing @state) "Stop" "Play")
                        :on-click #(swap! state update :playing not)}]]]

   (let [max-beats
         (map (comp
               (partial apply max)
               (partial map :beat)
               :pattern)
              (vals (:instruments @state)))

         loop-length
         (reduce pat/lcm (filter #(< 0 %) max-beats))

         live-notes (into #{}
                          (map (fn [m] (update m :beat #(+ 1 (mod (- % 1) loop-length))))
                               @live-notes))
         ]

     (for [instrument pat/instruments]
       [instrument-block {:key instrument :extend-to loop-length
                          :live-notes live-notes
                          }
        (reagent/cursor state [:instruments instrument])]))
   ]
  )

(reagent/render-component
 [ui/mui-theme-provider
  {:mui-theme (ui-core/get-mui-theme)}
  [main-component]]


 (. js/document (getElementById "app")))
