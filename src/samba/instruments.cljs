(ns samba.instruments
  (:require [samba.patch :as patch :include-macros true]))

(enable-console-print!)

(defn record-instrument [gain create-play-fn]
  (let [buffer-a (atom nil)
        buffer-b (atom nil)]
    (patch/record 1.5 (partial reset! buffer-a) (create-play-fn false))
    (patch/record 1.5 (partial reset! buffer-b) (create-play-fn true))

    (fn [time accent]
      (if-let [buffer @(if accent buffer-b buffer-a)]
        (patch/connect! (patch/sample {:envelope gain
                                       :buffer buffer :start time}))))))

(defn sos-bass [{f0 :frequency
                 beta :beta
                 sharpen :sharpen
                 gain :gain
                 decay :decay
                 }]
  (patch/at (patch/now)
            (let [modulator
                  (patch/sine {:frequency f0 :envelope (* f0 beta)
                               :start 0 :stop 1})

                  carrier
                  (patch/tri {:start 0 :stop 1
                              :frequency (list f0 modulator)})

                  hpf
                  (patch/highpass {:frequency 8000} carrier)

                  lpf
                  (patch/lowpass {:frequency [[0 10000] [0.1 200 :exp]]} hpf)

                  carrier-2
                  (patch/tri {:start 0 :stop 1
                              :frequency [[0 (* (+ 1 sharpen) f0)] [0.1 f0 :exp]]})

                  lpf-2
                  (patch/lowpass {:frequency 400} carrier-2)

                  noise
                  (patch/noise {:type :red :start 0
                                :envelope [[0 1] [0 [0 0.05] :tgt]]})

                  output
                  (patch/gain [[0 gain] [0 [0 decay] :tgt]] lpf lpf-2 noise)
                  ]

              (patch/connect! output))))

(def surdo-1
  (record-instrument
   0.6
   (fn [a] (sos-bass {:gain 1 :decay 0.2 :frequency 65 :beta 30
                      :sharpen (if a 0.25 0)}))))

(def surdo-2
  (record-instrument
   0.6
   (fn [a] (sos-bass {:gain 1 :decay 0.2 :frequency 75 :beta 20
                      :sharpen (if a 0.25 0)}))))

(def surdo-3
  (record-instrument
   0.7
   (fn [a] (sos-bass {:gain (if a 1.1 0.8) :decay 0.15 :frequency 90
                      :beta 10 :sharpen (if a 0.25 0)}))))

(def surdo-4
  (record-instrument
   0.7
   (fn [a] (sos-bass {:gain (if a 1.1 0.8) :decay 0.15 :frequency 100
                      :beta 10 :sharpen (if a 0.25 0)}))))

(def agogo
  (record-instrument
   0.3
   (fn [accent]
     (let [f (if accent :C6 :A6)
           f (if (keyword? f) (patch/scale f) f)
           trigger (patch/trigger)
           bell (patch/gain
                 (list 0 (trigger [[0 0.3] [0 [0 0.5] :tgt]]))
                 (patch/tri  {:envelope (trigger [[0 0.5]  [0 [0 0.25] :tgt]]) :start 0 :frequency (* f 0.25)})
                 (patch/tri  {:envelope (trigger [[0 0.75]  [0 [0 0.2] :tgt]]) :start 0 :frequency (* f 0.5)})
                 (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.2] :tgt]]) :start 0 :frequency f})
                 (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.18] :tgt]]) :start 0 :frequency (* f 2)})
                 (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.1] :tgt]]) :start 0 :frequency (* f 3)})
                 (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.08] :tgt]]) :start 0 :frequency (* f 4.2)})
                 (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.05] :tgt]]) :start 0 :frequency (* f 5.4)})
                 (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.025] :tgt]]) :start 0 :frequency (* f 6.8)})
                 )]

       (patch/connect! bell)
       (patch/fire trigger)))))

(defn bell-high [t a] (agogo t true))
(defn bell-low [t a] (agogo t false))

(def snare
  (record-instrument
   0.3
   (fn [accent]
     (let [trigger (patch/trigger)
           snare-node
           (patch/gain
            (list 0 (trigger #(case %
                                :accent
                                [[-0.01 0 :ramp] [0 0.5] [0.6 0]]
                                [[-0.01 0 :ramp] [0 0.3] [0.5 0]])))
            ;; another filtered triangle

            (patch/lowpass
             {:frequency 1000
              :envelope (trigger [[-0.01 0 :ramp] [0 0] [0.01 0.25 :ramp] [0.3 0.01 :exp]])}

             (patch/tri {:frequency 300 :start 0}))
            ;; a dash of noise
            (patch/highpass
             {:frequency 400}
             (patch/noise {:envelope
                           (trigger [[-0.01 0 :ramp] [0 1] [0.4 0 :exp]])
                           :type :white :start 0})))
           ]

       (patch/connect! snare-node)
       (patch/at 0.01 (patch/fire trigger (when accent :accent)))
       ))))

;; accented repi should have sharpened partials

(let [play-rep
      (fn [accent]
        (let [components
              [260 360 400 450  650 660 750 850 1050 1250 1350 1610 1810 2600 2500 2700]
              weights
              [1  0.5 0.5 0.75 0.5 0.4 0.3 0.2 0.1  0.1 0.1 0.05 0.01 0.01 0.01 0.01 ]

              oscillators
              (for [[comp wei] (map vector components weights)]
                (patch/tri {:frequency comp :envelope wei :start 0}))

              t (patch/trigger)

              noise
              (patch/noise {:type :red :start 0
                            :envelope (t #(case %
                                            :accent
                                            [[0 4.5] [0.05 [0 0.1] :tgt]]
                                            [[0 4] [0.05 [0 0.08] :tgt]]
                                            ))
                            })

              sum
              (apply patch/gain
                     (list 0 (t #(case %
                                   :accent
                                   [[0 0.4] [0.1 [0 0.1] :tgt]]
                                   [[0 0.3] [0 [0 0.1] :tgt]]
                                   )))
                     noise
                     oscillators)
              ]
          (patch/connect! sum)
          (patch/fire t (when accent :accent))))
      ]

  (def repi2
    (record-instrument 0.3 play-rep)))
