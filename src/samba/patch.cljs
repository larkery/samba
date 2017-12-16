(ns samba.patch)

(defprotocol Trigger
  (await [t f] "get t from v")
  )

(defprotocol Triggers
  (fire [t] [t a])
  )

(defn trigger []
  (let [values (atom ())]
    (reify cljs.core/IFn

      ;; construct a new Trigger which we can pass to patch functions
      ;; to listen to
      (-invoke [this value-or-fn]
        (let [listens (atom ())

              val-fn (if (or (vector? value-or-fn)
                             (not (fn? value-or-fn)))
                       (constantly value-or-fn)
                       value-or-fn)

              out (reify
                    Trigger
                    (await [this cb] (swap! listens conj cb))

                    Triggers
                    (fire [this]
                      (fire this nil)
                      )
                    (fire [this key]
                      (let [val (val-fn key)]
                        (doseq [li @listens] (li val)))))

              ]
          (swap! values conj out)
          out))

      Triggers
      (fire [t] (doseq [val @values] (fire val)))
      (fire [t a] (doseq [val @values] (fire val a)))
      )))

(defonce ^:dynamic *context*
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

(defonce ^:dynamic *time* 0)

(defn offline-context [duration & [channels sample-rate]]
  (let [channels (or channels 2)
        sample-rate (or sample-rate (.-sampleRate *context*))
        duration (* duration channels sample-rate)
        ]
    (js/OfflineAudioContext. channels duration sample-rate)))

(defn now [] (.-currentTime *context*))

(defn connect! [node]
  (.connect node (.-destination *context*)))

(def scale
  (let [root (Math/pow 2 (/ 1 12))
        a 440
        freq (fn [n] (* a (Math/pow root n)))
        note-name
        [ "A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"]
        ]
    (into
     {}
     (for [step (range 9)
           note (range 12)]
       [(keyword (str (note-name note) step))
        (freq (+ note (* 12 (- step 4))))]))))

(defn set-parameter!
  "Set the given AudioParam to the given value.
  If VALUE is a number, the value property of the AUDIO-PARAM is set.
  If VALUE is a vector, it's assumed to be a series of time/value pairs.
  The values are scheduled in time order.
  If VALUE is an AudioNode it is connected to the AudioParam."
  [audio-param value]
  (cond
    ;; a deferred value
    (satisfies? Trigger value)
    (await value #(set-parameter! audio-param %))

    ;; a series of control points
    (vector? value)
    (do
      (let [first-time (+ *time* (first (first value)))]
        (.cancelScheduledValues audio-param first-time))

      (doseq [[t v & [how]] value]
        (let [v (or (scale v) v)
              t (+ t *time*)]
          (case (or how :set)
            ;; settargetattime is pants, we have value and timeconstant
            :tgt (.setTargetAtTime audio-param (first v) t (second v))
            :set (.setValueAtTime audio-param v t)
            :ramp (.linearRampToValueAtTime audio-param v t)
            :exp (if (zero? v)
                   (do (.exponentialRampToValueAtTime audio-param 0.01 t)
                       (.linearRampToValueAtTime audio-param v (+ t 0.01)))
                   (.exponentialRampToValueAtTime audio-param v t))))))

    ;; a constant value or frequency like :A4
    (or (keyword? value) (number? value))
    (set! (.-value audio-param) (or (scale value) value))

    ;; an audionode
    (instance? js/AudioNode value)
    (.connect value audio-param)

    ;; several values (e.g. several audio nodes)
    (seq? value)
    (doseq [v value] (set-parameter! audio-param v))

    :otherwise
    (throw (str "Bad audio value" value))))


(declare gain)

(defn start-stop [node {start :start stop :stop duration :duration
                        envelope :envelope}]
  (when start (.start node (+ start *time*)))

  (cond
    stop (.stop node (+ *time* stop))
    duration (.stop node (+ *time* start duration)))

  (if envelope
    (gain envelope node)
    node))

(defn wave
  "Create an oscillator node, with :shape and :frequency.
  TODO support LFO to oscillate in a range."
  [{shape :shape
    frequency :frequency
    :as params}]
  (let [node (.createOscillator *context*)]
    (set! (.-type node) (name shape))
    (set-parameter! (.-frequency node) frequency)
    (start-stop node params)))

;; annoyingly there is no "sum" node type
;; nor a constant node type

(defn sine [params] (wave (assoc params :shape :sine)))
(defn tri [params] (wave (assoc params :shape :triangle)))
(defn saw [params] (wave (assoc params :shape :sawtooth)))
(defn square [params] (wave (assoc params :shape :square)))

(defn gain
  "Multiply-add all of the given inputs. Gain can be a value, a vector
  of values, or another audio-node"
  [gain & inputs]
  (let [node (.createGain *context*)]
    (set-parameter! (.-gain node) gain)
    (doseq [i inputs]
      (.connect i node))
    node))

(def noise-buffer
  (memoize
   (fn [type bs]
     (let [bf (.createBuffer *context* 2 bs
                             (.-sampleRate *context*))

           rnd #(- (* 2 (Math/random)) 1)

           generate-noise
           (case (or type :white)
             :white
             (fn [buf n]
               (loop [i 0]
                 (if (= i n)
                   buf
                   (do (aset buf i (rnd))
                       (recur (inc i))))))
             :red
             (fn [buf n]
               (loop [i 0
                      l 0]
                 (if (= i n)
                   buf
                   (let [l (/ (+ l (* (rnd) 0.02)) 1.02)]
                     (aset buf i (* 3.5 l))
                     (recur (inc i) l)))))
             :pink
             (fn [buf n]
               (loop [i 0
                      b0 0 b1 0 b2 0 b3 0 b4 0 b5 0 b6 0]
                 (if (= i n)
                   buf
                   (let [w (rnd)
                         b0 (+ (* b0 0.99886) (* w 0.0555179))
                         b1 (+ (* b1 0.99332) (* w 0.0750759))
                         b2 (+ (* b2 0.96900) (* w 0.1538520))
                         b3 (+ (* b3 0.86650) (* w 0.3104856))
                         b4 (+ (* b4 0.55000) (* w 0.5329522))
                         b5 (+ (* b5 -0.7616) (* w 0.0168980))
                         output (* 0.11 (+ b0 b1 b2 b3 b4 b5 b6 (* 0.5362 w)))
                         b6 (* w 0.115926)
                         ]
                     (aset buf i output)
                     (recur (inc i)
                            b0 b1 b2 b3 b4 b5 b6))))
               ))
           ]

       (generate-noise (.getChannelData bf 0) bs)
       (generate-noise (.getChannelData bf 1) bs)

       bf
       )
     )))

(defn noise
  "Make a noise-generator. A 2-second repeating noise buffer is used."
  [{type :type :as params}]
  (let [sr (.-sampleRate *context*)
        bs (* 1 sr)
        bf (noise-buffer type bs)
        n (.createBufferSource *context*)
        ]
    (set! (.. n -buffer) bf)
    (set! (.. n -loop) true)
    (start-stop n params)))

(defn sample
  "Play a sample from a buffer"
  [{buffer :buffer :as params}]
  (let [node (.createBufferSource *context*)]
    (set! (.. node -buffer) buffer)
    (start-stop node params)))

(defn filtr [{type :type
              frequency :frequency
              q :Q
              :or
              {type :lowpass
               frequency 1000}
              :as params
              }
             input]
  (let [filter (.createBiquadFilter *context*)]
    (set! (.-type filter) (name type))
    (set-parameter! (.-frequency filter) frequency)
    (when q (set-parameter! (.-Q filter) q))
    (.connect input filter)
    (start-stop filter params)))

(defn lowpass [params input]
  (filtr (assoc params :type :lowpass) input))

(defn highpass [params input]
  (filtr (assoc params :type :highpass) input))
