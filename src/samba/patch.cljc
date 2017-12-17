(ns samba.patch)

(defmacro at [time & stuff]
  `(binding [*time* (+ *time* ~time)]
     ~@stuff))

(defmacro with-context
  [context-value & stuff]
  `(binding [*context* ~context-value]
     ~@stuff))

(defmacro record [duration callback & stuff]
  `(let [ooc# (offline-context ~duration)]
     (with-context ooc# ~@stuff)
     (let [promise# (.startRendering ooc#)]
       (if promise#
         (.then promise# ~callback)
         (set! (.-oncomplete ooc#)
               (fn [ev#]
                 (~callback (.-renderedBuffer ev#))
                 ))
         ))))
