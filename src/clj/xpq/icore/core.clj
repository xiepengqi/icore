(ns clj.xpq.icore.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io File]))

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

(defn blank? [str]
  (if str
    (if (string? str)
      (= "" (str/trim str))
      nil)
    nil))

(defn not-blank? [str]
  (not (blank? str)))

(defmulti exist? class)
(defmethod exist? String [file-path]
  (let [file (io/as-file file-path)]
    (if file
      (.exists file)
      false)))
(defmethod exist? File [file]
  (if file
    (.exists file)
    false))

(defprotocol FileSys
  (child-files [file])
  (dir? [file]))
(extend-protocol FileSys
  String
  (child-files [file-path]
    (child-files (io/as-file file-path)))
  (dir? [file-path]
    (dir? (io/as-file file-path)))
  File
  (child-files [file]
    (.listFiles file))
  (dir? [file]
    (.isDirectory file)))


