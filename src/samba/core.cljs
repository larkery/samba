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

(defonce machine (sound/create-machine
                  {:S1 (sound/beep :freq :D2 :length 2)
                   :S2 (sound/beep :freq :F2 :length 2)
                   :S3 (sound/fuzz (sound/squares) :delta-time 0.01)
                   :S4 (sound/fuzz (sound/squares :freq 60) :delta-time 0.01)
                                        ;               :SN "/fx/snare-vinyl01.wav"
                   ;;:HP "/fx/snare-dist03.wav"
                   }))

(sound/set-patterns! machine (:instruments @state))

(defn classes [& cs] (string/join " " (filter identity cs)))

(defn instrument-pattern
  [{key :key} instrument pattern]
  [ui/paper
   {:key key}

   [:div.pattern
    {:style {:display :flex}}
    [:span.instrument-name
     {:style {:flex-shrink 0 :margin-right :4px :width :40px :text-align :right}
      :on-click #(sound/play! machine instrument)
      :role :button
      }
     instrument]
    [:span.notes {:style {:display :inline-flex :flex-wrap :wrap}}
     (let [by-beat (group-by :beat pattern)
           beats (sort (keys by-beat))
           ]
       (for [beat beats]
         (let [notes-in-beat (by-beat beat)]
           [:span {:key beat :style {:border-left "1px black solid"
                                     :display :inline-flex
                                     :width :100px
                                     :padding 0}
                   }
            (for [{type :type note :note} notes-in-beat]
              [:span {:key note
                      :style {:flex-grow 1 :flex-basis :1em
                              :text-align :center :margin :3px}}
               (case type :accent "⬤" :sound "⭘" :rest "-")
               ])])))]]])

(reagent/render-component
 [ui/mui-theme-provider
  {:mui-theme (ui-core/get-mui-theme)}
  [ui/paper {:style {:margin :1em}}
   [ui/raised-button {:label "Play"
                      :on-click #(sound/play! machine)}]
   [ui/raised-button {:label "Stop"
                      :on-click #(sound/stop! machine)}]

   (for [[i p] (:instruments @state)]
     [instrument-pattern {:key i} i p])]]

 (. js/document (getElementById "app")))
