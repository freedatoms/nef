(ns nef.nes.neat.species
  (:require [nef.nes.neat
             [evolution-parameters :as ep]]))

(defrecord Species
    [id
     members
     representative
     offspring
     age
     avg-fitness
     max-fitness
     max-fitness-prev
     no-improvement-age
     has-best])

(defn new-species
  [initial-member]
  (->Species 
   (swap! ep/species-count inc)
   [initial-member]
   initial-member
   0
   0
   0
   0
   0
   0
   false))
