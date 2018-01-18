(ns samba.patterns.samba-reggae
  (:require [samba.patterns :refer [drums surdos]]))

(def patterns                                                   ; MAIN PATTERNS:
  {:s1  {:1 '[_ _ _ _ | . _ _ _ ]                               ; surdo 1, pattern 1
         }
   :s2  {:1 '[. _ _ _ | _ _ _ _ ]                               ; surdo 2, pattern 2
         }
   :s3  {:1 '[. _ _ _ | _ _ _ _ | . . . . | . . . . |           ; surdo 3, pattern 1
              . _ _ _ | _ _ _ _ | _ _ _ _ | . . . .]
         }
   :s4  {:1  '[. _ _ _ | . _ ! _ | . _ _ _ | . ! _ !]           ; surdo 4, pattern 1
         :2  '[_ _ _ _ | _ . . _ ]                              ; surdo 4, pattern 2
         }
   :rep {:1 '[! . . ! | . . ! . | . . ! . | . ! . .]            ; repinique, pattern 1
         :3 '[! . . ! | . . ! . | ! . . ! | . . ! .]            ; repinique, pattern 3
         :5 '[! . . ! | . . . ! | . . ! . | ! . . .]            ; repinique, pattern 5
         }
   :cai {:1 '[. . ! .]                                          ; caixa, pattern 1
         }
   })

(def break-one                                                  ; BREAK ONE
  [
   {:mes '[6 . . . 4 . . | . _ 6 . . . 4 . . . _ ]}             ; Call
   {drums '[ . . . . | _ |]}                                    ; Response
   {:mes '[6 . . . 4 . . | . _ 6 . . . 4 . . . _ ]}             ; Call
   {drums '[ . . . . ]}                                         ; Response (excluding last .)
   ])

(def break-two                                                  ; BREAK TWO
  [{drums
    '[. . _ . | _ . _ . | _ . _ . | _ . _ . ]
    }])

(def horizontal-sticks                                          ; HORIZONTAL STICKS
  [{surdos                                                      ; Drum pattern:
    '[! | _ | _ | _ |                                           ; Dum!
      ! | _ | _ | _ |                                           ; Dum!

      ! _ _ ! | _ _ ! _ | _ _ ! _ | ! _ ! !                     ; dak-dak-dak dum dum da-dum

      | | | | |                                                 ; Wait for a bit

      3 . . . . . . . . . . . .                                 ; Lots of triplets
      ]
    :rep :continue                                              ; Everyone else continues
    :cai :continue
    }])

(def break-four                                                 ; BREAK FOUR
  [
                                                                ; First part:
   {#{:s1 :s2 :s3 :s4 :rep}                                     ; Everyone but caixa
    '[. . _ _ | _ _ . _ | . . _ _ | _ _ _ _ |                   ; dada - dak -dada
      . . _ _ | . _ . _ | . . _ _ | _ _ _ _ |                   ; dada - dak-dak - dada
      . . _ _ | _ _ . _ | . . _ _ | _ _ _ _ |                   ; dada -dak -dada
      . . _ _ | . _ . _ | . _ . _ ]                             ; dada dak dak dak dak
    :cai :continue                                              ; Caixa continues
    }
                                                                ; Second part:
   {:s3 '[. . . .]                                              ; Roll for 3rds, back in
    }
   ])

(def breaks {:1 break-one
             :2 break-two
             "'Horizontal Sticks'" horizontal-sticks
             :4 break-four
             })
