(ns samba.grid
  (:require [samba.patterns :as pat]
            [samba.sequencer :as sequencer]
            [reagent.core :as reagent]
            )
  )

(defn switches
  [{sel :selection
    opts :options
    on-change :on-change}]

  [:span.switches
   (for [[k v] opts]
     (if (= k sel)
       [:span.switch.selected {:key k} v]
       [:span.switch
        {:key k
         :on-click #(on-change k)
         :style {:cursor :pointer}
         :role :button}
        v]))])


(defn drum-line [{key :key
                  extended-from :extended-from} pattern]
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
                         :style {:color (when (= type :rest) :grey)
                                 :width (str (/ 8 time) "em")
                                 ;; :flex-grow (/ 1 time)
                                 }
                         }
             (case type :accent "!" :sound "•" :rest "-")]))]))])

(defn cue-bar [{title :title
                on-click :on-click
                }]
  [:div {:style {:display :flex :flex :flex-wrap :flex-direction :row :padding-bottom :0.2em
                 :align-items :center
                 :border-bottom "3px #aaa solid"}}

   [:div {:style {:font-variant :small-caps
                  :font-weight :bold
                  :font-size :1.5em}} title ":"]
   [:button.cue {:on-click on-click} "CUE"]])

(defn pattern-selector [params patterns]
  (let [selection (reagent/atom (into {} (for [[k v] patterns] [k (first (first v))])))]
    (fn [{extend-to :extend-to labels :labels s :sequencer} patterns]
      [:div
       [cue-bar {:title "Patterns"

                 :on-click
                 #(sequencer/cue-patterns!
                   s
                   (for [[k s] @selection]
                     [k (pat/complete-pattern (get-in patterns [k s]))]))
                 }]

       (doall
        (for [[k ps] patterns]
          (let [current-pattern (@selection k)
                other-patterns (keys ps)
                pattern-value (pat/complete-pattern (ps current-pattern))
                ]
            [:div.pattern-container
             {:key k}
             [:div.pattern-header
              [:b (or (labels k) (name k))]]
             [drum-line {:extended-from (apply max (map :beat pattern-value))}
              (pat/extend pattern-value extend-to)]
             [switches {:selection current-pattern
                        :options (for [p2 other-patterns] [p2 (name p2)])
                        :on-change #(swap! selection assoc k %)}]
             ])))
       ])))

(defn break-selector [{labels :labels sequencer :sequencer} breaks]
  [:div
   (doall
    (for [[break-name break-value] breaks]
      ;; TODO complete pattern once up here.
      [:div.break {:key break-name}
       [cue-bar {:title (if (string? break-name) break-name
                            (str "Break " (name break-name)))
                 :on-click #(sequencer/cue-break!
                             sequencer
                             (map (fn [break-unit]
                                    (into {}
                                          (for [[k p] break-unit]
                                            [k (if (= p :continue) p
                                                   (pat/complete-pattern p))])))
                                  break-value))
                 }]
       (for [[i break-element] (map-indexed vector break-value)]
         [:div.break-unit {:key i}
          (for [[k pat] break-element]
            [:div.pattern-container {:key k}
             [:div.pattern-header [:b (labels k)]]
             (if (= pat :continue)
               [:em "Continue pattern"]
               [drum-line {} (pat/complete-pattern pat)])])])]))
   ])


(defn drum-lines
  "Draw a set of drum lines, and also wire up a synthesizer to play them.
  PATTERNS contains the normal pattern set for the instruments, and BREAKS the breaks.
  In PATTERNS we map instrument name to a map from pattern name to pattern.
  In BREAKS we map break name to something more complicated.
  "

  [params patterns breaks]

  [:div
   [pattern-selector params patterns]
   [break-selector params breaks]]
  )
