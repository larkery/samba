(ns samba.sound
  (:require [goog.net.XhrIo :as xhr])
  )

(defn create-oscillator [c t]
  (let [osc (.createOscillator c)]
    (set! (.. osc -type) (name t))
    osc))

(defn schedule-values [audio-node plan]
  (doseq [[time value & [method]] plan]
    (case method
      :set (.setValueAtTime audio-node value time)
      :ramp (.linearRampToValueAtTime audio-node value time)
      :exp (.exponentialRampToValueAtTime audio-node value time)

      (.setValueAtTime audio-node value time))))

(defn create-gain [context & plan]
  (let [gain (.createGain context)]
    (schedule-values (.-gain gain) plan)
    gain))

(defn connect [& path]
  (reduce (fn [a b] (.connect a b)) path))

(def scale
  (let [root (Math/pow 2 (/ 1 12))
        a 440
        freq (fn [n] (* a (Math/pow root n)))
        note-name
        ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]
        ]
    (into
     {}
     (for [step (range 9)
           note (range 12)]
       [(keyword (str (note-name note) step))
        (freq (+ (- note 9) (* 12 (- step 4))))]))))

(defn copy [f n]
  (fn [c t g]
    (dotimes [_ n]
      (f c t g))))

(defn fuzz [f0 & {:keys [delta-time delta-gain]
                   :or {delta-time 0.05 delta-gain 0.05}}]
  (fn [c t g]
    (let [t (+ t (* delta-time (- (Math/random) 0.5)))
          g (+ g (* delta-gain (- (Math/random) 0.5)))]
      (f0 c t g))))

(defn beep [& {:keys [freq shape env length]
               :or {freq 16
                    shape :sine
                    env :exp
                    length 1}}]

  (let [freq (if (keyword? freq) (scale freq) freq)]
    (fn [context now g0]
      (let [end (+ now length)
            osc (create-oscillator context shape)
            gain (create-gain context [now g0] [end 0.001 env])
            ]
        (set! (.. osc -frequency -value) freq)
        (connect osc gain (.-destination context))
        (.start osc now)
        (.stop osc end)))))

(defn squares [& {:keys [freq ratios length]
                  :or {freq 40
                       length 0.1
                       ratios [2 3 4.16 5.43 6.79 8.21]}}]
  (fn [context t g]
    (let [gain (create-gain context [t g] [(+ t length) 0.0001 :exp])]
      (connect gain (.-destination context))
      (doseq [r ratios]
        (let [osc (create-oscillator context :square)
              freq (* freq r)
              ]
          (set! (.. osc -frequency -value) freq)
          (connect osc gain)
          (.start osc t)
          (.stop osc (+ t length 0.1))))
      )))


(defn kick-drum [& {:keys [f1 f2 t]
                    :or {f1 120
                         f2 50
                         t 0.5}}]
  (fn [context now gain]
    (let [dest (.-destination context)
          end (+ now t)

          tri (create-oscillator context :triangle)
          sine (create-oscillator context :sine)

          g-tri (create-gain context [now 1] [end 0.001 :exp])
          g-sine (create-gain context [now 1] [end 0.001 :exp])

          g-multi (create-gain context [now gain])
          ]

      (schedule-values
       (.-frequency tri)
       [[now f1] [end 0.001 :exp]])

      (schedule-values
       (.-frequency sine)
       [[now f2] [end 0.001 :exp]])

      (connect tri g-tri g-multi)
      (connect sine g-sine g-multi)
      (connect g-multi dest)

      (.start tri now)
      (.start sine now)
      (.stop tri end)
      (.stop sine end)
      )))

(defn sample-drum [sample-buffer]
  (fn [context now gain]
    (println "play a drum" sample-buffer now gain)
    (let [dest (.-destination context)
          source (.createBufferSource context)
          g-samp (create-gain context [now gain])]

      (set! (.. source -buffer) sample-buffer)
      (connect source g-samp dest)
      (.start source now)
      )))

(defn create-machine [samples]
  (let [audio-context (or js/window.AudioContext
                          js/window.webkitAudioContext)

        audio-context (audio-context.)

        machine
        (atom {:context audio-context
               :patterns {}
               :queue ()
               :samples {}
               :tempo 80
               :playing false
               })]
    ;; queue up loading of samples
    (doseq [[instrument rule] samples]
      (cond
        (fn? rule)
        (swap! machine assoc-in [:samples instrument]
               rule)

        (string? rule)
        (let [url rule
              x (goog.net.XhrIo.)]
          (.setResponseType x xhr/ResponseType.ARRAY_BUFFER)
          (.listen
           x
           goog.net.EventType.COMPLETE
           (fn [e]
             (let [wav-data (.. e -target -xhr_ -response)]
               (.decodeAudioData
                audio-context wav-data
                (fn [sound]
                  (println "Loaded sample for " instrument " from " url)
                  (swap! machine assoc-in [:samples instrument]
                         (sample-drum sound)
                         ))))))
          (.send x url))
        )
      )

    machine
    ))

(defn current-time [context] (.-currentTime context))

(defn play-sample! [machine sample]
  (let [{c :context s :samples} @machine]
    ((s sample) c (current-time c) 1)
    ))

(defn play-one-loop [machine start & instruments]
  (let [{samples :samples
         patterns :patterns
         context :context
         tempo :tempo} @machine

        instruments (into #{} (or instruments (keys patterns)))
        duration (atom 0)
        ]
    (doseq [[i pat] patterns]
      (when (instruments i)

        (let [sample (samples i)]

          (when sample
            (doseq [{beat :beat
                     time :time
                     note :note
                     type :type
                     :as n} pat]
              (when-not (= type :rest)

                (let [time (* 60 (/ (+ (- beat 1) (/ note time)) tempo))]
                  (swap! duration max beat)
                  (sample context (+ start time) (case type :sound 0.5 :accent 0.75))
                  ))

              ))))
      )
    (+ start (/ (* 60 @duration) tempo))
    ))

(defn start-playing [machine]
  (let [next-loop
        (atom (current-time (:context @machine)))

        trigger
        (fn trigger []
          (let [st @machine]
            (when (:playing st)
              (let [now @next-loop
                    next (play-one-loop machine now)
                    delta (- next now)]
                (reset! next-loop next)
                (js/setTimeout trigger (* 1000 (- delta 1)))))))
        ]
    (trigger)))


(defn play! [machine & instruments]
  (swap! machine assoc :playing true)
  (start-playing machine))

(defn stop! [machine]
  (swap! machine assoc :playing false))

(defn set-patterns! [machine patterns]
  (swap! machine assoc :patterns patterns))

(defn mute! [machine instrument])
