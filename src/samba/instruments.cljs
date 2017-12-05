(ns samba.instruments
  (:require [samba.patch :as patch :include-macros true]))

(enable-console-print!)

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




(let [trigger (patch/trigger)
      env (trigger [[-0.01 0 :ramp]
                    [0 1]
                    [0.5 0.0 :exp]])

      fm-1 (fmod
            {:start 0
             :modulator {:shape :sine
                         :envelope (trigger [[0 1000] [0.4 500 :exp]])
                         :frequency (trigger [[0 300] [0.4 270 :exp]])}
             :carrier {:shape :sine :frequency (trigger 440)}
             :envelope env
             :filter {:Q 0.5
                      :frequency (trigger [[0 4000] [0.4 2000 :exp]])}
             })

      fm-2 (fmod
            {:start 0
             :modulator {:shape :sine
                         :frequency (trigger [[0 300] [0.1 150 :exp]])
                         :envelope (trigger [[0 4000] [0.1 1500 :exp]])}
             :carrier {:shape :sine :frequency 450}
             :envelope env
             :filter {:Q 3 :frequency (trigger [[0 2000] [0.3 1000 :exp]])}
             }
            )

      noise (patch/gain
             env
             (patch/filtr
              {:type :lowpass :frequency (trigger [[0 8000]
                                                   [0.25 4000 :exp]])}
              (patch/noise {:type :white :start 0})))
      ]
  (patch/connect! (patch/gain (list 0
                                    (trigger #(case %
                                                :accent
                                                [[0 0.3] [0.8 0]]
                                                [[0 0.1] [0.8 0]]))
                                    ) fm-1 fm-2 noise))

  (defn repi [start accent]
    (patch/at start (patch/fire trigger (when accent :accent))))
  )

(let [trigger (patch/trigger)
      env (trigger [[-0.01 0 :ramp]
                    [0 1]
                    [0.5 0.0 :exp]])

      fm-1 (fmod
            {:start 0
             :modulator {:shape :sine
                         :envelope (trigger [[0 1000] [0.4 500 :exp]])
                         :frequency (trigger [[0 300] [0.4 270 :exp]])}
             :carrier {:shape :sine :frequency (trigger 440)}
             :envelope env
             :filter {:Q 0.5
                      :frequency (trigger [[0 4000] [0.4 2000 :exp]])}
             })

      fm-2 (fmod
            {:start 0
             :modulator {:shape :sine
                         :frequency (trigger [[0 300] [0.1 150 :exp]])
                         :envelope (trigger [[0 4000] [0.1 1500 :exp]])}
             :carrier {:shape :sine :frequency 450}
             :envelope env
             :filter {:Q 3 :frequency (trigger [[0 2000] [0.3 1000 :exp]])}
             }
            )

      noise (patch/gain
             env
             (patch/filtr
              {:type :lowpass :frequency (trigger [[0 8000]
                                                   [0.25 4000 :exp]])}
              (patch/noise {:type :white :start 0})))
      ]
  (patch/connect! (patch/gain (list 0
                                    (trigger #(case %
                                                :accent
                                                [[0 0.3] [0.8 0]]
                                                [[0 0.1] [0.8 0]]))
                                    ) fm-1 fm-2 noise))

  (defn leader [start accent]
    (patch/at start (patch/fire trigger (when accent :accent)))
    )
  )


(defn surdo [f0]
  (let [trigger (patch/trigger)
        fm-1 (fmod {:start 0
                    :modulator {:shape :sine
                                :frequency 1000
                                ;; is there an issue here caused by the phase?
                                :envelope (trigger [[0 10] [0.5 5 :exp]])
                                }
                    :carrier {:shape :sine :frequency f0}
                    :envelope (trigger [[-0.01 0 :ramp] [0 1] [1 0 :exp]])
                    :filter {:Q 2 :frequency 300 :type :lowpass}
                    })

        noise
        (patch/filtr
         {:type :lowpass
          :frequency (trigger [[0 5000] [0.25 4000 :exp]])
          :envelope (trigger [[-0.01 0 :ramp] [0.01 0.75 :ramp] [0.5 0.0 :exp]])
          }
         (patch/noise {:type :red :start 0}))
        ]

    (patch/connect!
     (patch/gain
      (list 0 (trigger #(case %
                          :accent
                          [[-0.01 0 :ramp] [0 1] [1 0]]
                          [[-0.01 0 :ramp] [0 0.8] [1 0]])
                       ))
      fm-1 noise))

    (fn [start is-accent]
      (patch/at start (patch/fire trigger (when is-accent :accent))))))

(def surdo-1 (surdo 50))
(def surdo-2 (surdo 60))
(def surdo-3 (surdo 75))
(def surdo-4 (surdo 85))

(let [trigger (patch/trigger)
      snare-node
      (patch/gain
       (list 0 (trigger #(case %
                           :accent
                           [[-0.01 0 :ramp] [0 0.5] [0.6 1]]
                           [[-0.01 0 :ramp] [0 0.3] [0.5 0]])))
       ;; another filtered triangle

       (patch/lowpass
        {:frequency 1000
         :envelope (trigger [[-0.01 0 :ramp] [0 0] [0.01 0.25 :ramp] [0.3 0 :exp]])}

        (patch/tri {:frequency 300 :start 0}))
       ;; a dash of noise
       (patch/highpass
        {:frequency 400}
        (patch/noise {:envelope
                      (trigger [[-0.01 0 :ramp] [0 1] [0.4 0 :exp]])
                      :type :white :start 0})))
      ]

  (patch/connect! snare-node)
  ;; so put! and so on doesn't work
  ;; because bindings don't work.
  (defn snare [time accent]
    (patch/at time (patch/fire trigger (when accent :accent))))
  )
