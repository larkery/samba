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


;; (let [trigger (patch/trigger)
;;       env (trigger [[-0.01 0 :ramp]
;;                     [0 1]
;;                     [0.5 0.0 :exp]])

;;       fm-1 (fmod
;;             {:start 0
;;              :modulator {:shape :sine
;;                          :envelope (trigger [[0 1000] [0.4 500 :exp]])
;;                          :frequency (trigger [[0 300] [0.4 270 :exp]])}
;;              :carrier {:shape :sine :frequency 500}
;;              :envelope env
;;              :filter {:Q 0.5
;;                       :frequency (trigger [[0 4000] [0.4 2000 :exp]])}
;;              })

;;       fm-2 (fmod
;;             {:start 0
;;              :modulator {:shape :sine
;;                          :frequency (trigger [[0 300] [0.1 150 :exp]])
;;                          :envelope (trigger [[0 4000] [0.1 1500 :exp]])}
;;              :carrier {:shape :sine :frequency 450}
;;              :envelope env
;;              :filter {:Q 3 :frequency (trigger [[0 2000] [0.3 1000 :exp]])}
;;              }
;;             )

;;       noise (patch/gain
;;              env
;;              (patch/filtr
;;               {:type :lowpass :frequency (trigger [[0 8000]
;;                                                    [0.25 4000 :exp]])}
;;               (patch/noise {:type :white :start 0})))
;;       ]
;;   (patch/connect! (patch/gain (list 0
;;                                     (trigger #(case %
;;                                                 :accent
;;                                                 [[0 0.3] [0.8 0]]
;;                                                 [[0 0.1] [0.8 0]]))
;;                                     ) fm-1 fm-2 noise))

;;   (defn repi [start accent]
;;     (patch/at start (patch/fire trigger (when accent :accent))))
;;   )


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

    (fn [start is-accent]
      (patch/at start (patch/fire trigger (when is-accent :accent))))))

(def surdo-1 (surdo 60))
(def surdo-2 (surdo 70))
(def surdo-3 (surdo 80))
(def surdo-4 (surdo 90))

(defn bell [f]
  (let [f (if (keyword? f) (patch/scale f) f)

        trigger (patch/trigger)
        bell (patch/gain
              (list 0 (trigger [[0 1] [0 [0 0.5] :tgt]]))
              (patch/tri  {:envelope (trigger [[0 0.5]  [0 [0 0.25] :tgt]]) :start 0 :frequency (* f 0.25)})
              (patch/tri  {:envelope (trigger [[0 0.75]  [0 [0 0.2] :tgt]]) :start 0 :frequency (* f 0.5)})
              (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.2] :tgt]]) :start 0 :frequency f})
              (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.18] :tgt]]) :start 0 :frequency (* f 2)})
              (patch/tri  {:envelope (trigger [[0 1]  [0 [0 0.1] :tgt]]) :start 0 :frequency (* f 3)})
              (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.08] :tgt]]) :start 0 :frequency (* f 4.2)})
              (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.05] :tgt]]) :start 0 :frequency (* f 5.4)})
              (patch/sine {:envelope (trigger [[0 1]  [0 [0 0.025] :tgt]]) :start 0 :frequency (* f 6.8)})
              )

        ]
    (patch/connect! bell)
    (fn [start is-accent]
      (patch/at start (patch/fire trigger)))))

(def bell-high (bell :C5))
(def bell-low (bell :A5))

(defn agogo [start is-accent]
  (if is-accent
    (bell-high start is-accent)
    (bell-low start is-accent)))

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


;; RHS 260, 360, 400, 450, 650, 660, 750, 850, 1050, 1250, 1350, 1610, 1810, 2600, 2500, 2700
;; (let [fundamental 265
;;       ;; first 450 (* 1.69 ish)
;;       ;;     210,  230, 180, all really transient and gone after about 0.17s
;;       ]


;;   (defn repi-2 [time accent]

;;     ))

;; accented repi should have sharpened partials
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
                           [[0 0.2] [0 [0 0.1] :tgt]]
                           )))
             noise
             oscillators)
      ]
  (patch/connect! sum)
  (defn repi2 [time accent]
    (patch/at time (patch/fire t (when accent :accent)))
    )
  )


;; (defn fm [{frequency :frequency ;; fundamental
;;            beta :beta ;; the modulation index
;;            cmr :cmr ;; carrier-to-modulator ratio}
;;            }]
;;   (let [modulator (patch/wave something)
;;         carrier (patch/wave something)
;;         ])
;;   )
