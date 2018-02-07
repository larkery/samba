(ns samba.patterns.aforxe
  (:require [samba.patterns :refer [drums surdos]]))

(def patterns                                                     ; MAIN PATTERNS
                                                                  ; All Surdos:
  {surdos {:1 '[h h . _]                                          ;    Pattern 1
           :2 '[h . . _]                                          ;    Pattern 2
           :5 '[h h . _ | h h . _ | h h . h | . h . h]            ;    Pattern 5
           }
   :cai {:1 '[. . * .]                                            ; Caixa pattern 1
         }
   :rep {:1 '[* _ _ _]                                            ; Repinique pattern 1

                                                                  ; Repinique pattern 2:
         :2 '[* * _ _ | _ _ _ _ | * _ * _ | _ _ _ _               ; First time round
              * * _ _ | _ _ _ _ | 6 * . . 4 * _ | 6 * . . 4 * _
              * * _ _ | _ _ _ _ | * _ * _ | _ _ _ _
              * * _ _ | _ _ _ _ | 3 * * * | 4 * _ _ _             ; <- has three-time

              * * _ _ | _ _ _ _ | * _ * _ | _ _ _ _               ; Second time round
              * * _ _ | _ _ _ _ | 6 * . . 4 * _ | 6 * . . 4 * _
              * * _ _ | _ _ _ _ | * _ * _ | _ _ _ _
              * * _ _ | _ _ _ _ | * _ * _ | * _ * _               ; <- has half-time
              ]
         }
   :agg {:1 '[* * _ . | _ . . _ | * _ * _ | . _ . _ ]             ; Agogo bells, pattern 1
                                                                  ; Horrendous ear-worm
         }
   })

(def break-one                                                    ; BREAK ONE
  [                                                               ; Part one:
   {:agg  '[* * _ . | _ . . _ | * _ * _ | . _ . _ ]               ;  Just Agogo bells (like pattern 1)
    }
                                                                  ; Part two:
   {:agg  '[* * _ . | _ . . _ | * _ * _ | . _ . _ ]               ;   Bells keep going
    drums '[_       | _       | * _ * _ | * _ * _ ]}              ;   Drums play along with last 4
   ])

(def break-two                                                    ; BREAK TWO
  [{drums '[. _ _ . | _ _ . _ | . _ _ . | _ _ . _ ]               ; Everyone plays 2-bar clave,
    :agg :continue                                                ; except bells which keep going
    }])

(def break-three                                                  ; BREAK THREE
  [{drums '[. _ . . | _ . . _ |                                   ; Da dada dada
            . _ . . | _ . . _ |                                   ; Da dada dada
            . _ . . | _ . . _ | * _ _ _ |                         ; Da dada dada DAK
            _ |                                                   ; Wait a mo
            . _ . . | _ . . _ |                                   ; Da dada dada
            . _ . . | _ . . _ |                                   ; Da dada dada
            . _ . . | _ . . _ | * _ *  _ | * _ * _                ; Da dada dada DAK DAK DAK DAK
            ]
    :agg :continue}                                               ; And more bells at same time
   ])

(def break-pointy-downy
  [{#{:rep :cai}
    '[ * . . * | . . * . | . . * . | * . . . ] ; son clave
    ;; 4 bars of stuff
    }
   {:rep '[ * . . * | . . * . | . . * . | * . . . ]
    ;; 8 bars of stuff
    }
   ])

(def breaks
  {"Break 1 (repeat x4)" break-one
   :2 break-two
   :3 break-three})
