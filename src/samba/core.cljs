(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [samba.sound :as sound]
            [clojure.string :as string]

            [cljsjs.material-ui]
            [cljs-react-material-ui.core :as ui-core]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defn pattern-for-everyone [pattern-name]
  ;; TODO do the cycle completion thing neater
  (pat/complete-patterns (pat/patterns pattern-name)))

(defonce state (reagent/atom
                {:instruments
                 (pattern-for-everyone :R1)
                 }))

(def machine (sound/create-machine
              {:S1 "/fx/kick-acoustic01.wav"
               :S2 "/fx/kick-acoustic02.wav"
               :S3 "/fx/tom-short.wav"
               :S4 "/fx/snare-vinyl01.wav"
               :SN "/fx/snare-dist01.wav"
               :HP "/fx/snare-dist02.wav"
               }))

(defn classes [& cs] (string/join " " (filter identity cs)))

(defn instrument-pattern
  [{key :key} instrument pattern extend-to]
  (let [pattern (pat/extend pattern extend-to)]
    [ui/paper
     {:key key}

     [:div.pattern
      [:span.instrument-name
       {:style {:width :40px
                :display :inline-block
                :text-align :right
                :margin-right :-40px}}
       instrument]

      (let [by-beat (group-by :beat pattern)
            beats (sort (keys by-beat))
            ]
        (for [beat beats]
          (let [notes-in-beat (by-beat beat)]
            [:span {:key beat :style {:border-left
                                      (when (> beat 1) "1px black solid")
                                      :margin-left :45px
                                      :display :inline-flex
                                      :width :90px
                                      :padding 0}
                    }
             (for [{type :type note :note} notes-in-beat]
               [:span {:key note
                       :style {:flex-grow 1 :flex-basis 0
                               :text-align :center :margin :3px}}
                (case type
                  :accent "⬤"
                  :sound "⭘"
                  :rest "-"
                  )]

               )


             ]
            )
          )
        )

      ]
     ])
  )

(reagent/render-component
 [ui/mui-theme-provider
  {:mui-theme (ui-core/get-mui-theme)}
  [ui/paper {:style {:margin :1em}}

   (let [longest-pattern (reduce pat/lcm
                                 (map (fn [p] (apply max (map :beat p)))
                                      (vals (:instruments @state))))
         ]
     (println "Extend patterns to " longest-pattern)
     (for [[i p] (:instruments @state)]
       [instrument-pattern {:key i} i p longest-pattern]))]]

 (. js/document (getElementById "app")))
