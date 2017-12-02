(ns samba.patch)

(defmacro at [time & stuff]
  `(binding [*time* (+ *time* ~time)]
     ~@stuff))
