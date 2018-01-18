(ns samba.patterns.six-eight
  (:require [samba.patterns :refer [drums surdos]]))

(def patterns                                                ; MAIN PATTERNS
  {:s1 {:1 '[3 _ _ _ | . _ _]                                ; Firsts, pattern 1
        }
   :s2 {:1 '[3 . _ _ | _ _ _]                                ; Seconds, pattern 1
        }
                                                             ; TODO: Can't remember thirds
   :s4 {:1 '[3 . . . | _ _ _ | _ _ _ | _ _ _]                ; Fourths, pattern 1
        }
   :rep {:1 '[3 ! . ! | . ! ! | . ! . | ! ! . ]              ; Repinique, pattern 1
         }
   :cai {:1 '[3 . . .]                                       ; Caixa, pattern 1
         }
   }
  )
