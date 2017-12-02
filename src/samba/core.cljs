(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [clojure.string :as string]
            [samba.grid :as grid]
            [samba.patch :as patch]
            [samba.instruments :as instruments]
            [cljsjs.material-ui]
            [cljs-react-material-ui.core :as ui-core]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defn classes [& cs] (string/join " " (filter identity cs)))

(defn box
  [title & contents]

  (into
   [ui/paper {:style {:margin-bottom :1em
                      :padding :0.5em}}
    [:h3 title]]
   contents))


(let [accent (atom false)]
  (defn instrument-description [name sound & description]
    (into [:li name " " [:button {:role :button
                                  :on-click #(sound (patch/now) (swap! accent not))}
                         "🔉"] ". "]
          description))
  )

(defonce instrs
  {:s1 instruments/surdo-1
   :s2 instruments/surdo-2
   :s3 instruments/surdo-3
   :s4 instruments/surdo-4
   :cai instruments/snare
   :rep instruments/repi
   :mes instruments/leader})

(defn page []
  [:div
   [box "Bristol Samba - My Notes"
    [:p "This page contains Tom's notes about Samba drumming.
It can also play the patterns as a reminder.
The drums are synthesised rather than recorded, so they sound a bit off. "
     "The bass drums sound the same without good speakers or headphones."]]
   [box "Instruments"
    [:p "In Samba so far we have played six instruments:"]
    [:ul {:style {:margin-left :1em}}
     [instrument-description
      "The caixa, or snare drum"
      (instrs :cai)
      "This makes an enharmonic sound with a burst of noise in it from the wire springs."]
     [instrument-description
      "The repinique."
      (instrs :rep)
      "This is more harmonic than the caixa, and makes a ringing sound."]
     [instrument-description
      "The 1st surdo"
      (instrs :s1)
      "This is a two-skinned bass drum, and has the lowest note."]
     [instrument-description
      "The 2nd surdo"
      (instrs :s2)
      "Like the 1st, but higher."]
     [instrument-description
      "The 3rd surdo"
      (instrs :s3)
      "Again, higher than the 2nd."]
     [instrument-description
      "The 4th surdo"
      (instrs :s4)
      "The highest bass drum, still lower than the repinique or caixa."]

     [instrument-description "Leader"
      (instrs :mes)
      "I don't know the name of this. It sounds a bit like the repinique."]]]

   [box "Patterns"
    [:p "The music we have played so far is composed of patterns of rhythm."]
    [:p "The patterns are in families, like " [:em "Samba Reggae"] " and within each family each drum has its own patterns, identified by numbers."]
    [:p "A pattern is a series of notes on the drum. Some notes are " [:em "accents"] " which means they are louder. "
     "Within the pattern, time is divided into " [:em "beats"] ". "
     "Most of the patterns we have learned are 1, 2, 4 or 8 beats long (before they repeat themselves). "
     "Each beat is chopped up into smaller intervals. Often, there are four of these in each beat - they are called " [:em "sixteenths"] " because there are 16 of them in the four beats. "
     "In some of the patterns we do triplets instead, dividing each beat into three."
     ]

    [:p "Here's a picture of a pattern which has a note on each beat, and an accent on the first and third beats:"]
    [grid/drum-lines
     {:extend-to 4 :instruments instrs}
     {:s1 {:1 '[! _ _ _ . _ _ _]}}
     ]

    [:p "The vertical lines divide the beats. Each sounded note has a dot, and each rest a dash. Dots with a box around them are accents so they are louder. Beats 3 and 4 are a bit grey because they repeat 1 and 2."]]

   [box "Samba Reggae"
    [:p
     "Samba Reggae is the first family of patterns we have seen. "
     "It is to a count of 4, and the notes are all on sixteenths. "]

    [grid/drum-lines
     {:instruments instrs :extend-to 4}
     {:s1  {:1 '[_ _ _ _ | . _ _ _ ]}
      :s2  {:1 '[. _ _ _ | _ _ _ _ ]}
      :s3  {:1 '[. _ _ _ | _ _ _ _ | . . . . | . . . . | . _ _ _ | _ _ _ _ | _ _ _ _ | . . . .]}
      :s4  {:1 '[. _ _ _ | . _ ! _ | . _ _ _ | . ! _ !]
            :2 '[_ _ _ _ | _ . . _ ]}
      :rep {:1 '[! . . ! | . . ! . | . . ! . | . ! . .]
            :3 '[! . . ! | . . ! . | . . ! . | ! . . .]}
      :cai {:1 '[. . ! .]}
      }
     ]
    ]

   [box "Samba Funk"
    [:p
     "Samba funk is the second family of patterns we have seen. "
     "It is to a count of 4, with sixteenth notes. " "Many of the notes sound on the off-beat. "]
    [grid/drum-lines
     {:instruments instrs :extend-to 4}
     {:s1 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
      :s2 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
      :s3 {:1 '[_ _ _ _ | _ _ _ _ | _ _ _ . | . _ . _]}
      :s4 {:1 '[. . . . | . _ _ _ | _ _ _ _ | _ _ _ _]}
      :rep {:1 '[_ _ _ _ | . _ . _ | . _ _ _ | . _ _ _]} ;; TODO maybe wrong
      :cai {:1 '[. . ! .]}}]
    ]

   [box "Six-Eight"
    [:p
     "Six-eight is the third family we have seen. " "It is to a count of 4, with triplets. "]
    [grid/drum-lines
     {:instruments instrs :extend-to 4}
     {:s1 {:1 '[3 _ _ _ | . _ _]}
      :s2 {:1 '[3 . _ _ | _ _ _]}
;      :s3 {}
      :s4 {:1 '[3 . . . | _ _ _ | _ _ _ | _ _ _]}
;      :rep {} ;; the pattern I can't remember
      :cai {:1 '[3 . . .]}
      }
     ]
    ]
   ]
  )

(reagent/render-component
 [ui/mui-theme-provider
  {:mui-theme (ui-core/get-mui-theme)}
  [page]]

 (. js/document (getElementById "app")))
