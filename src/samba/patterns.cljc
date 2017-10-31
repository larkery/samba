(ns samba.patterns)

(def samba-reggae-patterns
  '{
    :surdo-1
    {1 [. _ _ _ | _ _ _ _
        . _ _ _ | _ _ _ _]}
    :surdo-2
    {1 [_ _ _ _ | . _ _ _
        _ _ _ _ | . _ _ _]}
    :surdo-3
    {1 [. _ _ _ | _ _ _ _
        . . . . | . . . .
        . _ _ _ | _ _ _ _
        _ _ _ _ | . . . .
        ]
     ;; not sure if this is quite right
     ;; 3:44 in video
     ;; da da da da dum boop
     ;; da da da da da da da dum boop boop
     }
    :surdo-4
    {1 [. _ _ _ | . _ ! _
        . _ _ _ | . ! _ !]

     2 [_ _ _ _ | _ . . _
        _ _ _ _ | _ . . _]
     }

    :snare ;; with the off beat, forever
    {1 [ . . ! . ]

     }

    :reppaniki
    {1 [! . . ! | . . ! .
        . . ! . | ! . . .]
     ;; John called this samba-reggae pattern 1 on 30th oct ?
     2 [! . . ! | . . ! .
        . . ! . | . ! . .]
     }

    :breaks
    {1 {} ;; break one goes here (da da da dum da da dum da da dum dum dum -> ba ba ba bum, same -> ba ba ba ba bum)
     :horizontal
     {:surdo
      [!       |         |         |         |
       ! _ _ _ | _ _ _ _ | _ _ _ _ | _ _ _ _
       ! _ ! _ | ! _ _ ! | _ ! _ ! | ! _ _ _
       3 . . . | . . . | . . . | . . . 4
       ]

      }
     }
    }
  )

(defn expand-pattern
  "Interprets a pattern of the form shown above.
Result is a vector of notes, each having :time in beats and :accent true/false.
Instrument should be silent the rest of the time."
  [pattern]
  (let [pattern
        (if (= '| (last pattern)) pattern
            (conj pattern '|))]
    (:pattern
     (reduce (fn [{beat :beat time :time pattern :pattern
                   :as state} sym]
               (case sym
                 (. !)
                 (assoc state
                        :beat (+ beat (/ 1 time))
                        :pattern (conj pattern {:time beat :accent (= sym '!)}))

                 (_)
                 (assoc state
                        :beat (+ beat (/ 1 time)))

                 (|)
                 (assoc state :beat (int (Math/ceil beat)))

                 (if (number? sym)
                   (assoc state
                          :time sym ;; change time sig
                          :beat (int (Math/ceil beat)) ;; also a bar line
                          )
                   state)))
             {:beat 1 :time 4 :pattern []}
             pattern))))
