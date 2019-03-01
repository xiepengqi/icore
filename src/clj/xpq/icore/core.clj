(ns clj.xpq.icore.core)

(defmacro <-
  "reverse ->>"
  [& args]
  (let [elems (reverse (reduce conj [] args))]
    (loop [x (first elems), forms (next elems)]
      (if forms
        (let [form (first forms)
              threaded (if (seq? form)
                         (with-meta `(~(first form) ~@(next form)  ~x)
                                    (meta form))
                         (list form x))]
          (recur threaded (next forms)))
        x))))

