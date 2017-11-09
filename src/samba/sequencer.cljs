(ns samba.sequencer
  (:require [samba.sound :as sound])
  )

;; who should own which bits?
;; I mean, I can have some watch that updates the sequencer
;; maybe that's preferable
(defn create-sequencer []
  (atom {:fx {}
         :patterns {}
         :tempo 70
         :playing false
         :animations ()}))

(defn- run-sequence [sequencer beat beat-time animations]
  (let [{playing :playing
         tempo :tempo
         fx :fx
         patterns :patterns
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
                    (swap! animations
                           ;; todo can we put these into a queue or something
                           conj
                           {:animation-time (+ beat-time sound-time)
                            :beat beat :time time :note note
                            :instrument instrument})

                    (comment
                      (effect (+ sound-time beat-time) gain)))

                  )))))
        ;; play next beat later with a bit of slack
        (let [next-beat-time (+ seconds-per-beat beat-time)]
          (js/setTimeout
          #(run-sequence sequencer
                         (+ 1 beat)
                         next-beat-time
                         animations)
          (- (* 1000 (- next-beat-time (sound/current-time))) 400)
          ))))))

(defn- play [sequencer upd set-highlights]
  (let [was-playing (:playing @sequencer)]
    (swap! sequencer update :playing upd)
    (when (and (:playing @sequencer) (not was-playing))
      (let [now (sound/current-time)
            animations (atom ())
            frame-start (js/performance.now)
            ]
        (run-sequence sequencer 1 now animations)
        ;; also trigger animatoins
        (js/requestAnimationFrame
         (fn paint [frame-time]
           (let [{playing :playing tempo :tempo} @sequencer
                 one-beat-time (/ 60 tempo)
                 ]
             (when playing
               (swap! animations
                      (fn [events]
                        (let [wall-time (/ (- frame-time frame-start) 1000)

                              recent? (fn [{note-time :time
                                            t :animation-time :as evt}]
                                        (let [dtt (/ one-beat-time note-time)
                                              dt (- (- t now) wall-time)]
                                          (< (- dtt) dt)))

                              past? (fn [{t :animation-time :as evt}]
                                        (let [dt (- (- t now) wall-time)]
                                          (< dt 0)))

                              keep (filter recent? events)
                              ]
                          (set-highlights (filter past? keep))
                          (into () keep))
                        ))
               (js/requestAnimationFrame paint))
             )))
        ))))


(defn set-playing! [sequencer playing render]
  (play sequencer (constantly playing) render))

(defn toggle-playing! [sequencer]
  (play sequencer not (fn [])))

(defn set-patterns! [sequencer patterns]
  (swap! sequencer assoc :patterns patterns))

(defn set-fx! [sequencer fx]
  (swap! sequencer assoc :fx fx))
