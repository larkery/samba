(ns samba.patterns.ile-aiye
  (:require [samba.patterns :refer [drums surdos]])
  )

(def patterns
  {:rep {:1 '[_ _ * _]
         :2 '[. . * . | . * . . | * . * . |
              . 6 * . * 4 .]
         }}
  )

;; [* _ _ * | _ _ _ * | _ _ * _ | * _ _ _
;;  * * _ * | _ * _ * | _ * * * | * _ _ _ ]
