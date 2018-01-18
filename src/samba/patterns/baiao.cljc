(ns samba.patterns.baiao
  (:require [samba.patterns :refer [drums surdos]]))

(def patterns                                                ; MAIN PATTERNS
  {:agg {:1 '[* _ _ _ | . _ _ _]                             ; Agogo pattern 1
         :2 '[_       | . _ . _ | _ * _ * | . _ . _]         ; Agogo pattern 2
         }

   :cai {:1 '[* . . * | . . * . | . * * . | * . * .]         ; Caixa pattern 1
         }

   :rep {:1 '[. . . . | * . * . | . * . . | * . * . ]        ; Repinique pattern 1
         :2 '[. . . . | * . * . | . * . . | * . * * ]        ; Repinique pattern 2
         }

   :s4 {:1 '[h h . _]                                        ; Fourths, pattern 1
        :2 '[. _ _ _ | . _ * _ | . _ _ _ | . _ * *]          ; Fourths, pattern 2
        }

   #{:s1 :s2 :s3}                                            ; Other surdos
   {:1 '[. _ _ . | _ _ _ _ |  . . _ . | _ _ _ _]             ;  Pattern 1
    :2 '[. _ _ _ | . _ _ _ | _ | _ ]                         ; Pattern 2
    }

   ;; Missing triangle and shaker for now
   ;; I think there's an accent on the off-beat?
   ;;:tri {:1 '[. . * .]}
   })

(def break-1-1st                                             ; BREAK ONE (first go)
  [{:agg :continue                                           ; Bells keep going
    :cai :continue                                           ; Caixa likewise
    :rep :continue                                           ; And repinique
    surdos                                                   ; Drums play half of the pulse
    '[* _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |                ; TODO (should this be * _ _ *)
      * _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |
      * _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |
      * _ _ _ | * _ _ _ | * * _ * | _ * * _                  ; And then have a twiddle at the end
                                                             ; TODO not sure about my timing in this
      ]
    }])

(def break-1-2nd                                             ; BREAK ONE (second go)
                                                             ; Same as above, except...
  [{:agg :continue
    :cai :continue
    (conj surdos :rep)                                       ; the repinique joins the surdos
    '[* _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |                ; TODO I am missing the twiddle for
      * _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |                ;      the repinique, unfortunately
      * _ _ _ | * _ _ _ | _ _ _ _ | _ _ _ _ |
      * _ _ _ | * _ _ _ | * * _ * | _ * * _
      ]
    }])

(def sock-puppet                                             ; SOCK PUPPET BREAK
  [{:agg '[* _ _ _ | . _ _ _ | * _ _ _ | . _ _ _ ]}          ; Just bells
   {:agg '[* _ _ _ | . _ _ _ | * _ _ _ | . _ _ _ ]
    :rep :continue}                                          ; Bring in repinique
   {:agg '[* _ _ _ | . _ _ _ | * _ _ _ | . _ _ _ ]
    :rep :continue :cai :continue}                           ; Bring in caixa
   {:agg '[* _ _ _ | . _ _ _ | * _ _ _ | . _ _ _ ]
    :rep :continue :cai :continue :s4 :continue}             ; Bring in 4ths

                                                             ; (implicitly bring in everyone)
   ])

(def breaks
  {"Break 1 (first time)" break-1-1st
   "Break 1 (second time)" break-1-2nd
   "Sock puppet" sock-puppet})
