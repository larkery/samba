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

(defn fmod
  [{start :start
    duration :duration
    mod-params :modulator
    carrier-params :carrier
    filter-params :filter
    envelope :envelope}]
  (patch/at start
            (let [mod-params
                  (merge
                   {:shape :triangle :frequency 200
                    :envelope [[0 1] [duration 0 :exp]]
                    :start 0 :duration duration
                    }
                   mod-params)

                  carrier-params
                  (merge {:shape :triangle :start 0 :duration duration}
                         carrier-params)

                  filter-params
                  (merge {:type :lowpass :frequency 1000} filter-params)

                  envelope
                  (or envelope [[0 1] [duration 0 :expt]])

                  modulator
                  (patch/wave mod-params)

                  carrier
                  (patch/wave (merge
                               carrier-params
                               {:frequency (list
                                            (or (:frequency carrier-params) 440)
                                            modulator)}))

                  filter
                  (patch/filtr filter-params carrier)
                  ]
              (patch/gain envelope filter))))

(defn surdo [f0]
  (record-instrument
   0.25
   (fn [is-accent]
     (println "recording surdo" f0)
     (let [trigger (patch/trigger)
        fm-1 (fmod {:start 0
                    :modulator {:shape :sine
                                :frequency f0
                                ;; is there an issue here caused by the phase?
                                :envelope (trigger [[0 10]
                                                    [0 [5 0.1] :tgt]
                                                    ;; [0.5 5 :exp]
                                                    ])
                                }
                    :carrier {:shape :sine :frequency f0}
                    :envelope (trigger [;; [-0.01 0 :ramp]
                                        [0 1]
                                        [0 [0 0.2] :tgt]
                                        ;[1 0 :exp]

                                        ])
                    :filter {:Q 2 :frequency 300 :type :lowpass}
                    })

        noise
        (patch/filtr
         {:type :lowpass
          :frequency (trigger [[0 5000]
                               [0 [4000 0.05] :tgt]
                               ;; [0.25 4000 :exp]
                               ])
          :envelope (trigger [;; [-0.01 0 :ramp]
                              [0 0.75]
                              [0.01 [0 0.05] :tgt]
                              ;[0.5 0.0 :exp]
                              ])
          }
         (patch/noise {:type :red :start 0}))
        ]

    (patch/connect!
     (patch/gain
      (list 0 (trigger #(case %
                          :accent
                          [[-0.01 0 :ramp] [0 1.5] [2 0]]
                          [[-0.01 0 :ramp] [0 1.2] [2 0]])
                       ))
      fm-1 noise))
    (patch/at 0.01 (patch/fire trigger (when is-accent :accent)))))))

(def surdo-1 (surdo 60))
(def surdo-2 (surdo 70))
(def surdo-3 (surdo 80))
(def surdo-4 (surdo 90))

(def agogo
  (record-instrument
   0.3
   (fn [accent]
     (let [f (if accent :C5 :A5)
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
   0.4
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
    (record-instrument 0.25 play-rep)))



;; (defn fm [{frequency :frequency ;; fundamental
;;            beta :beta ;; the modulation index
;;            cmr :cmr ;; carrier-to-modulator ratio}
;;            }]
;;   (let [modulator (patch/wave something)
;;         carrier (patch/wave something)
;;         ])
;;   )
