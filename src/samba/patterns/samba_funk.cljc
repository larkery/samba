(ns samba.patterns.samba-funk
  (:require [samba.patterns :refer [drums surdos]]))


(def patterns                                                     ; MAIN PATTERNS
  {:s1 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]               ; Firsts, pattern 1
        }
   :s2 {:1 '[. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]               ; Seconds, pattern 1
        }
   :s3 {:1 '[_ _ _ _ | _ _ _ _ | _ _ _ . | # _ . _]               ; Thirds, pattern 1
        }
   :s4 {:1 '[. . . . | . _ _ _ | _ _ _ _ | _ _ _ _]               ; Fourths, pattern 1
        }
   :rep {:1? '[_ _ _ _ | #  _ . . | _ _ _ _ | # _ _ _]            ; Repinique, pattern 1 (maybe?)
         }
   :cai {:1 '[. . # .]                                            ; Caixa, pattern 1
         }
   })

(def break-one                                                    ; BREAK ONE
  [                                                               ; Only one part
   {:rep '[6 . . . _ _ . | . . _ _ . . | 4 # _ # _ # _ # #]       ; Repiniques
    surdos  '[_ | _ |   # _ # _ # _ _ _]                          ; Surdos follow end of repiniques
    }])

(def breaks
  {:1 break-one})
