(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [clojure.string :as string]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defn pattern-for-everyone [pattern-name]
  ;; TODO do the cycle completion thing neater
  (pat/complete-patterns (pat/patterns pattern-name)))

(defonce state (reagent/atom
                {:instruments
                 (pattern-for-everyone :R1)
                 }
                ))

(defn classes [& cs]
  (string/join " " (filter identity cs)))

(defn pattern-blocks [notes]
  [:div.pattern
   (for [[i {beat :beat note :note type :type time :time}] (map-indexed vector notes)]
     [:span.note
      {:key i ;; TODO key is a bit pants
       ;; TODO width is wrong for non 4-4
       :style {:width (str (/ 4 time) "em")}
       :class (classes
               (name type)
               (when (= note time) "beat-end")
               (when (= note 1) "beat-start"))}
      ])
   ]
  )

(defn patterns []
  [:div
   [:h1 "Patterns"]

   [:div.buttons
    (for [pat (keys pat/patterns)]
      [:button {:key patm
                :on-click #(swap! state
                                  update :instruments
                                  merge
                                  (pattern-for-everyone pat)
                                  )
                } pat])
    ]

   (let [patterns (:instruments @state)]
     (for [instrument pat/instruments]
       (let [pattern (patterns instrument)]
         [:div.instrument
          {:key instrument :style {:display :flex}}
          [:div.name (str instrument)]
          [:select
           {:on-change (fn [e] (let [value (.. e -target -value)]


                                 ))}

           (for [[p is] pat/patterns
                 :when (is instrument)]
             [:option {:key p :value p} (str p)])
           ]

          [pattern-blocks pattern]

          ]
         )))]
  )

(reagent/render-component
 [patterns]

 (. js/document (getElementById "app")))
