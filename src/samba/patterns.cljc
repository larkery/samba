(ns samba.patterns)

(let [make-rests
      (fn  [from-frac beat time]
        (into [] (for [i (range 0 (/ (- 1 from-frac)
                                     (/ 1 time)))
                       ]
                   {:beat beat :note (+ from-frac (/ i time))
                    :time time :type :rest}))
        )

      reducer
      (fn [{pattern :pattern
            time :time
            beat :beat
            note :note
            skip :skip-bar}
           command]
        (cond
          ('#{. _ ! h} command)
          (let [next-note (+ note (/ 1 time))
                end-note (>= next-note 0.999)
                ]
            {:pattern (conj pattern
                            {:beat beat :time time :note note
                             :type (case command
                                     . :sound ! :accent
                                     :rest)
                             :style (when (= command 'h) :hand)
                             })
             :note (if end-note 0 next-note)
             :beat (if end-note (+ 1 beat) beat)
             :skip-bar end-note
             :time time})

          (number? command)
          {:pattern pattern
           :beat beat
           :note note
           :time command}

          (= '| command)
          {:pattern (if skip pattern (into [] (concat pattern (make-rests note beat time))))
           :beat (if skip beat (+ 1 beat))
           :note 0
           :time time}
          )
        )]

  (defn complete-pattern [pattern]
    (:pattern
     (reduce reducer
             {:pattern [] :time 4 :beat 1 :note 0 :skip-bar true}
             pattern)))

  )

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (if (or (zero? a) (zero? b))
    (max a b)
    (/ (* a b) (gcd a b))))

(defn extend
  ""
  [pat len]
  (if (seq pat)
    (let [pat-beats (group-by :beat pat)
          max-beat (apply max (keys pat-beats))
          len (max len max-beat)
          output
          (apply concat
                 (for [i (range 0 len)]
                   (let [beats-i (pat-beats (+ 1 (mod i max-beat)))]
                     (for [note beats-i]
                       (assoc note :beat (+ 1 i))))))
          ]
      output
      )
    (extend (for [n (range 4)]
              {:beat 1 :note (/ n 4)
               :type :rest :time 4}
              ) len)
    )
  )


(defn extend-patterns
  [patterns]

  (let [beat-maxima (remove #{0}
                            (for [[_ ns] patterns] (apply max (map :beat ns))))
        max-beat (reduce lcm beat-maxima)
        ]
    (for [[i p] patterns]
      [i (extend p max-beat)])))

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

   (for [[i p] patterns] [i (complete-pattern p)])

   extend-patterns

   ;; cycle patterns to fit

   (into {})))
