(ns nef.nes.neat.individual
  (:require [nef.nes.neat
             [genome :as genome]
             [evolution-parameters :as ep]]
            [rhizome
             [viz :as viz]])
  (:use [nef
         graphviz-enabled]))

(defrecord Individual
    [id
     genome
     raw-fitness
     fitness
     solved?
     success-rate]
  GraphvizEnabled
  (save-image [this filename]
    (save-image (:genome this) filename))
  (save-dot [this filename]
    (save-dot (:genome this) filename))
  (view [this]
    (view (:genome this) (viz/create-frame (format "Individual raw-fitness=%f fitness=%f"
                                                   (:raw-fitness this)
                                                   (:fitness this)))))
  (view [this frame]
    (view (:genome this) frame))
  (view [this frame title]
    (view (:genome this) frame title)))

(defn new-individual
  [&{:keys [genome input output id]}]
  (if (not (or genome (and input output)))
    (throw (Error. "Either genome or input and output must be supplied")))
  (->Individual (or id (swap! ep/individuals-count inc))
                (or genome (genome/initial-genome input output)) 0.0 0.0 false 0.0))

