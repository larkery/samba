(ns samba.sequencer
  (:require [samba.patch :as patch]))


;; What behaviour do I want?
;; if you cue 3 things a row, what should happen?

(defn create-sequencer
  [state & [mk-atom]]

  ((or mk-atom atom)
   (merge
    {:fx {}
     :tempo 100
     :queue #queue []
     :return-pattern nil
     :current-pattern nil
     :is-playing false
     }
    state)))

(defn- is-beat-1 [pattern beat]
  ;; TODO this is wrong if they don't cycle together
  (let [max (partial apply max)
        maxen (for [[_ p] pattern] (max (if (seqable? p)
                                          (map :beat p)
                                          '(1))))
        max-beat (max maxen)]
    (= 0 (mod (- beat 1) max-beat))))

(defn- shift-queued-patterns! [s beat]
  ;; so need to look at current-pattern
  (let [{[current-pattern-type current-pattern] :current-pattern
         queue :queue} @s
        should-shift
        (or (not current-pattern)
            (and (or (seq queue) (= current-pattern-type :break))
                 (is-beat-1 current-pattern beat)))
        ]
    ;; TODO keep instruments playing in the break
    (when should-shift
      (swap! s
             #(let [q (doall (:queue %))
                    new-pattern (or (peek q) (:return-pattern %))

                    rest-patterns (pop q)

                    shifted
                    (-> %
                        (assoc :current-pattern new-pattern)
                        (assoc :queue rest-patterns))
                    ]

                (if (= current-pattern-type :pattern)
                  (assoc shifted :return-pattern [current-pattern-type (into {} current-pattern)])
                  shifted)

                )))
    should-shift
    ))


(defn- play-notes! [s beat at-time]
  (let [{tempo :tempo
         fx :fx
         [_ return-to] :return-pattern
         [_ patterns] :current-pattern} @s

        seconds-per-beat (/ 60 tempo)
        ]
    (doseq [[instrument pattern] patterns]

      (let [pattern
            (if (= pattern :continue)
              (return-to instrument)
              pattern)
            ]

        (when (seq pattern)
         (let [beat (+ 1 (mod (- beat 1)
                              (apply max (map :beat pattern))))
               pattern (filter #(and (= beat (:beat %))
                                     (not (= :rest (:type %)))) ;; TODO here is for filtering accents
                               pattern)]
           (doseq [{type :type time :time note :note} pattern]
             (let [note-time (+ at-time (* seconds-per-beat (/ (- note 1) time)))]
               ;; shift onto animation queue for later

               ;; trigger playing the sound
               (fx instrument note-time (= type :accent)))
             ))))
      )))

(defn- schedule-beat! [s beat at-time]
  (let [{is-playing :is-playing
         tempo :tempo} @s

        seconds-per-beat (/ 60 tempo)]

    (when is-playing
      ;; 1: update the patterns in s
      (let [beat (if (shift-queued-patterns! s beat) 1 beat)]
        ;; 2: schedule the notes in the current beat
        (play-notes! s beat at-time)
        ;; 3: run schedule future beats
        (let [next-beat-time (+ seconds-per-beat at-time)]
          (js/setTimeout
           #(schedule-beat! s (+ 1 beat) next-beat-time)
           (- (* 1000 (- next-beat-time (patch/now))) 600))))
      )))


(defn- play! [s]
  (let [must-start (atom false)]
    (swap! s
           (fn [{is-playing :is-playing
                 queue :queue
                 :as state}]
             (if (and (not is-playing)
                      (not (empty? queue)))

               ;; trigger playback
               (do
                 (reset! must-start true)
                 (assoc state :is-playing true))

               ;; do nothing
               state)))

    (when @must-start

      ;; schedule some notes!!!!!
      (let [music-clock (patch/now)
            graphics-clock (js/performance.now)
            ]

        (schedule-beat! s 1 (+ 0.1 music-clock)))
      )))

(defn cue-break! [s break]
  ;; the break is a series of patterns that
  ;; need interpretation?
  ;; (swap! s update :queue conj patterns)
  (let [units (map #(vector :break %) break)]
    (swap! s update :queue
           into units))

  (play! s))

(defn cue-patterns! [s patterns]
  (swap! s update :queue conj [:pattern patterns])
  (play! s))

(defn stop! [s]
  (swap! s
         (fn [s]
           (-> s
               (assoc :is-playing false)
               (assoc :current-pattern nil)
               (assoc :return-pattern nil)
               (assoc :queue #queue [])))))
