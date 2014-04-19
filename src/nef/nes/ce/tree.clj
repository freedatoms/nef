(ns nef.nes.ce.tree
  (:require [clojure
             [zip :as z]]))

(def ^:dynamic *binary-function* ['seq 'par])
(def ^:dynamic *unary-function* ['val- 'incbias 'val+ 'decbias 'wait])
(def ^:dynamic *nullary-function* ['end 'rec])


(declare binary unary nullary)

(defn- max-binary [size]
  ((rand-nth [binary unary])  size))

(defn- binary [size]
  (let [left (inc (rand-int (- size 2)))
        right (- size left 1)]
    (list (rand-nth *binary-function*)
          (case left
            1 (nullary left)
            2 (unary left)
            (max-binary left))
          (case right
            1 (nullary right)
            2 (unary right)
            (max-binary right)))))

(defn- nullary [size]
  (list (rand-nth *nullary-function*)))

(defn- unary [size]
  (let [s (dec size)]
    (case  s
      1 (list (rand-nth *unary-function*) (nullary s))
      2 (list (rand-nth *unary-function*) (unary s))
      (list (rand-nth *unary-function*) (max-binary s)))))

(defn generate-tree
  [size]
  {:pre [(> size 0)]}
  (case size
    1 (nullary 1)
    2 (unary 2)
    ((rand-nth [binary unary]) size)))
