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

(defmacro fn-let
  "fn with let"
  [& elems]
  (loop [before `(), after elems]
    (if (vector? (first after))
      (let [before (reverse (conj before (first after)))
            after (next after)]
        `(fn ~@before
           (let ~(first after)
             ~@(next after))))
      (recur (conj before (first after)) (next after)))))

(defmacro defn-let
  "defn with let"
  [& elems]
  (loop [before `(), after elems]
    (if (vector? (first after))
      (let [before (reverse (conj before (first after)))
            after (next after)]
        `(defn ~@before
           (let ~(first after)
             ~@(next after))))
      (recur (conj before (first after)) (next after)))))






