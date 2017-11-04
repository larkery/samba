(ns samba.sound
  (:require [goog.net.XhrIo :as xhr])
  )

(defn create-machine [samples]
  (let [audio-context (or js/window.AudioContext
                          js/window.webkitAudioContext)

        audio-context (audio-context.)

        machine
        (atom {:context audio-context
               :patterns {}
               :queue ()
               :samples {}
               })]
    ;; queue up loading of samples
    (doseq [[instrument url] samples]
      (let [x (goog.net.XhrIo.)]
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
                (swap! machine assoc-in [:samples instrument] sound))))))
        (.send x url)))

    machine
    ;;
    ))

(defn play! [machine])
(defn stop! [machine])
(defn set-patterns! [machine])
(defn mute! [machine instrument])
