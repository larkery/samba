(ns samba.sound
  (:require [goog.net.XhrIo :as xhr])
  )

(defonce *context*
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

(defn output [] (.-destination *context*))

(defn create-oscillator [t]
  (let [osc (.createOscillator *context*)]
    (set! (.. osc -type) (name t))
    osc))

(defn schedule-values [audio-node plan]
  (doseq [[time value & [method]] plan]
    (case method
      :set (.setValueAtTime audio-node value time)
      :ramp (.linearRampToValueAtTime audio-node value time)
      :exp (.exponentialRampToValueAtTime audio-node value time)

      (.setValueAtTime audio-node value time))))

(defn create-gain [& plan]
  (let [gain (.createGain *context*)]
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
    (fn [now g0]
      (let [end (+ now length)
            osc (create-oscillator shape)
            gain (create-gain [now g0] [end 0.001 env])
            ]
        (set! (.. osc -frequency -value) freq)
        (connect osc gain (output))
        (.start osc now)
        (.stop osc end)))))

(defn squares [& {:keys [freq ratios length]
                  :or {freq 40
                       length 0.1
                       ratios [2 3 4.16 5.43 6.79 8.21]}}]
  (fn [t g]
    (let [g (/ g (count ratios))
          gain (create-gain [t g] [(+ t length) 0.0001 :exp])]
      (connect gain (output))
      (doseq [r ratios]
        (let [osc (create-oscillator :square)
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
  (fn [now gain]
    (let [dest (output)
          end (+ now t)

          tri (create-oscillator :triangle)
          sine (create-oscillator :sine)

          g-tri (create-gain [now 1] [end 0.001 :exp])
          g-sine (create-gain [now 1] [end 0.001 :exp])

          g-multi (create-gain [now gain])
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

(defn sample [url]
  (let [buffer (atom nil)
        request (goog.net.XhrIo.)]

    (.setResponseType request xhr/ResponseType.ARRAY_BUFFER)
    (.listen request
             goog.net.EventType.COMPLETE
             (fn [e]
               (.decodeAudioData
                *context*
                (.. e -target -xhr_ -response)
                #(reset! buffer %))))

    (.send request url)

    (fn [now gain]
      (if-let [sample-buffer @buffer]
        (let [dest (output)
              source (.createBufferSource *context*)
              g-samp (create-gain [now gain])]
          (set! (.. source -buffer) sample-buffer)
          (connect source g-samp dest)
          (.start source now))
        (println "Missing sample for" url now gain)
        ))
    ))

(defn current-time [] (.-currentTime *context*))
