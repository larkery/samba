(ns samba.sequencer
  (:require [samba.sound :as sound])
  )

;; who should own which bits?
;; I mean, I can have some watch that updates the sequencer
;; maybe that's preferable
(defn create-sequencer []
  (atom {:fx {}
         :patterns {}
         :tempo 60
         :playing false
         :animations ()}))

(defn- run-sequence [sequencer beat beat-time start-time]
  (let [{playing :playing
         tempo :tempo
         fx :fx
         patterns :patterns
         animations :animations
         } @sequencer]

    (when-not playing (println "Stopping playback"))
    (when playing
      ;; get the current time from the audio context
      ;; get the next beats for each non-muted instrument
      ;; schedule the beats
      ;; need to avoid scheduling the beat if it's too far in the future

      (let [seconds-per-beat (/ 60 tempo)]
        (doseq [[instrument pattern] patterns]
          (when (seq pattern)
            (when-let [effect (fx instrument)]
              (let [max-beat (apply max (map :beat pattern))
                    beat-mod (+ 1 (mod beat max-beat))
                    notes (filter #(and
                                    (not (= :rest (:type %)))
                                    (= beat-mod (:beat %)))
                                  pattern)
                    ]
                (doseq [{type :type time :time note :note} notes]
                  (let [fraction (/ (- note 1) time)
                        sound-time (* fraction seconds-per-beat)
                        gain (case type :accent 1 :sound 0.6)]
                    ;; schedule to play this note
                    (swap! sequencer
                           update :animations
                           conj {:animation-time (+ beat-time sound-time)
                                 :beat beat :time time :note note}
                           )
                    (effect (+ sound-time beat-time) gain))
                  )))))
        ;; play next beat later with a bit of slack
        (let [next-beat-time (+ seconds-per-beat beat-time)]
          (js/setTimeout
          #(run-sequence sequencer
                         (+ 1 beat)
                         next-beat-time
                         start-time)
          (- (* 1000 (- next-beat-time (sound/current-time))) 400)
          ))))))

(defn- play [sequencer upd]
  (let [was-playing (:playing @sequencer)]
    (swap! sequencer update :playing upd)
    (when (and (:playing @sequencer) (not was-playing))
      (let [now (sound/current-time)]
        (swap! sequencer assoc :play-start (js/performance.now))

        (run-sequence sequencer 1 now now)
        (js/requestAnimationFrame
         (fn paint [animation-time]
           (let [{playing :playing
                  animations :animations} @sequencer]
             (when playing
               (js/requestAnimationFrame paint)))))
        ))))


(defn set-playing! [sequencer playing]
  (play sequencer (constantly playing)))

(defn toggle-playing! [sequencer]
  (play sequencer not))

(defn set-patterns! [sequencer patterns]
  (swap! sequencer assoc :patterns patterns))

(defn set-fx! [sequencer fx]
  (swap! sequencer assoc :fx fx))
