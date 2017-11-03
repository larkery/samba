(ns samba.patterns)

(def instruments
  [:M
   :SN
   :HP
   :S1
   :S2
   :S3
   :S4])

(def patterns
  (->
   '{:R1 {:S1 [_ _ _ _ | . _ _ _]
          :S2 [. _ _ _ | _ _ _ _]
          :S3 [. _ _ _ | _ _ _ _ | . . . . | . . . .
               . _ _ _ | _ _ _ _ | _ _ _ _ | . . . .]
          :S4 [. _ _ _ | . _ ! _ | . _ _ _ | . ! _ !]
          :SN [. . ! .]
          :HP [! . . ! | . . ! . | . . ! . | ! . . .] ;; maybe?
          }

     :R2 {:S4 [_ _ _ _ | _ . . _ ]
          :HP [! . . ! | . . ! . | . . ! . | . ! . .] ;; maybe?
          }

     :F1 {:S1 [. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]
          :S2 [. _ _ _ | _ _ _ _ | . _ . _ | _ _ _ _]
          :S3 [_ _ _ _ | _ _ _ _ | _ _ _ . | . _ . _]
          :S4 [. . . . | . _ _ _ | _ _ _ _ | _ _ _ _]
          :SN [. . ! .]
          ;; :HP ?????
          }

     :B1 {} ;; break one here

     :BH {[:S1 :S2 :S3 :S4]
          [! _ _ _ | _ _ _ _ | _ _ _ _ | _ _ _ _
           ! _ _ _ | _ _ _ _ | _ _ _ _ | _ _ _ _
           ! _ ! _ | ! _ _ ! | _ ! _ ! | ! _ _ _
           3 . . . | . . . | . . . | . . . 4
           ]
          }
     }

   )
  )



(let [make-rests
      (fn  [from beat time]
        (into [] (for [i (range from (+ 1 time))]
                   {:beat beat :note i
                    :time time :type :rest}))
        )

      reducer
      (fn [{pattern :pattern
            time :time
            beat :beat
            note :note
            skip :skip-bar}
           command]
        (if ('#{. _ !} command)
          (let [next-note (+ note 1)]
            {:pattern (conj pattern
                            {:beat beat :time time :note note :type
                             (case command _ :rest . :sound ! :accent)})
             :note (if (> next-note time) 1 next-note)
             :beat (if (> next-note time) (+ 1 beat) beat)
             :skip-bar (> next-note time)
             :time time})
          ;; otherwise
          (let [next-time (if (number? command) command time)]
            ;; this is a bit wrong; missing rests is too big often.
            {:pattern (if skip pattern
                          (into [] (concat pattern (make-rests note beat time))))
             :beat (+ 1 beat)
             :note 1
             :time next-time}
            )))]

  (defn complete-pattern [pattern]
    (:pattern
     (reduce reducer
             {:pattern [] :time 4 :beat 1 :note 1 :skip-bar true}
             pattern)))

  )

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn extend-patterns
  [patterns]

  (let [lcm (reduce lcm (map count (remove empty? (vals patterns))))]
    (for [[i p] patterns]
      [i (take lcm (cycle p))])))

(defn complete-patterns
  "Given a dictionary of patterns, complete them by:
1. Expanding them to include any missing rests
2. Cycling them until they are of equal length

The notes in the pattern are agumented to include:
:beat, the integral beat to which they belong
:note, the subdivision to which they belong
:time, the current time signature (denominator of note)
:type [:rest :note :accent]

The notes can't be scheduled directly from here because of fractional
times."
  [patterns]
  (->>
   ;; complete each pattern

   (for [[i p] patterns]
     [i (do (println "completing" i)
            (complete-pattern p))])

   extend-patterns

   ;; cycle patterns to fit

   (into {})))
