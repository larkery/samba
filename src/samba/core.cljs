(ns samba.core
  (:require [reagent.core :as reagent]
            [samba.patterns :as pat]
            [clojure.string :as string]
            [samba.grid :as grid]
            [samba.patch :as patch]
            [samba.instruments :as instruments]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defonce instrs
  {:s1 instruments/surdo-1
   :s2 instruments/surdo-2
   :s3 instruments/surdo-3
   :s4 instruments/surdo-4
   :cai instruments/snare
   :rep instruments/repi2
   :mes instruments/leader
   :agg instruments/agogo
   })

(let [accent (atom false)]
  (defn sound-button [sound]
    [:button {:role :button
              :on-click #(sound (patch/now) (swap! accent not))}
     "🔉"]))

(def blanks
  {:caixa-sound [sound-button instruments/snare]
   :repi-sound [sound-button instruments/repi2]
   :s1-sound [sound-button instruments/surdo-1]
   :s2-sound [sound-button instruments/surdo-2]
   :s3-sound [sound-button instruments/surdo-3]
   :s4-sound [sound-button instruments/surdo-4]
   :agogo-high [sound-button instruments/bell-high]
   :agogo-low [sound-button instruments/bell-low]

   :example-pattern
   [grid/drum-lines
    {:extend-to 4 :instruments instrs}
    {:s1 {:1 '[! _ _ _ . _ _ _]
          :2 '[. _ ! _ . ! _ !]}}
    ]

   :reggae-patterns
   [grid/drum-lines
    {:instruments instrs :extend-to 4}

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
     [{:mes '[. . . _ | . . . _ | . . . _ | . _ . _ ]}
      {:all '[ . . . . ]}
      {:mes '[. . . _ | . . . _ | . . . _ | . _ . _ ]}
      {:all '[ . . . . ]}
      ]
     :4
     [{#{:s1 :s2 :s3 :s4 :rep}
       '[. . _ _ | _ _ . _ | . . _ _ | _ |
         . . _ _ | . _ . _ | . . _ _ | _ |
         . . _ _ | _ _ . _ | . . _ _ | _ |
         . . _ _ | . _ . _ | . _ . _ | _]}
      {:s3 '[. . . .]}
      ]
     }

    
    ]

   :funk-patterns
   [grid/drum-lines
    {:instruments instrs :extend-to 4}
    {:s1 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
     :s2 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]}
     :s3 {:1 '[_ _ _ _ | _ _ _ _ | _ _ _ . | ! _ . _]}
     :s4 {:1 '[. . . . | . _ _ _ | _ _ _ _ | _ _ _ _]}
     :rep {:1 '[_ _ _ _ | . _ . _ | . _ _ _ | . _ _ _]} ;; TODO maybe wrong
     :cai {:1 '[. . ! .]}}]

   :six-eight-patterns
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

   :aforche-patterns
   [grid/drum-lines
    {:instruments instrs :extend-to 4}
    {:s3 {:1 '[_ _ . _]
          :2 '[_ . . _]
          }
     :agg {:1 '[! ! _ . | _ . . _ | ! _ ! _ | . _ . _ ]}
     } ; TODO accents vs hands
    ]
   }

  
  )

(doseq [[k v] blanks]
  (when-let [element (js/document.getElementById (name k))]
    (reagent/render-component v element)))
