(ns nef.nes.ce.population
  (:require [nef.nes.ce
             [individual :as i]]))

(defn make-population [n]
  (vec (repeatedly n i/new-individual)))

(defn simple-selection [population n]
  (take n (sort-by :fitness > population)))

(defn tournament-selection [population k]
  (let [selected (sort-by :fitness > (repeatedly k #(rand-nth population)))]
    (first selected)))
