(ns nef.nes.ce.individual
  (:gen-class)
  (:require [nef.nes.ce
             [tree :as t]]
            [nef.nes.ce [cellular-encoding-j :refer [ce->neural-net]]]
            [rhizome.viz :as viz]
            [clojure
             [zip :as z]])
  (:use nef.graphviz-enabled))

(set! *warn-on-reflection* true)

(def ^:dynamic *genome-length*
  "The initial number of nodes in genome tree."
  10)

(def ^:dynamic *max-genome-length*
  "The maximum number of nodes in genome tree."
  20)

(def ^:dynamic *mutation-rate*
  "Mutation rate is probability that allele mutates"
  0.005)

(def ^:dynamic *fitness-function* "Fitness function for evolution")

(defrecord ^{:doc "An individual is represented by the genome and the fitness"}
    Individual
  [genome fitness]
    GraphvizEnabled
    (save-image [this filename]
      (save-image (:genome this) filename))
    (save-dot [this filename]
      (save-dot (:genome this) filename))
    (view [this]
      (view (:genome this) (viz/create-frame (str "Genome {fitness " (:fitness this) "}"))))
    (view [this frame]
      (view (:genome this) frame))
    (view [this frame title]
      (view (:genome this) frame title)))

(defn new-individual
  "Creates new random individual"
  [& {:keys [genome-length genome fitness]
      :or {genome-length *genome-length*}}]
  (if (and genome fitness)
    (->Individual genome fitness)
    (let [g (or genome (t/generate-tree genome-length))
          fitness  (*fitness-function* g)]
      (->Individual g fitness))))

(defn mutate [ind]
  (let [genome (:genome ind)
        size (dec (count (flatten genome)))
        bin (into #{} t/*binary-function*)
        una (into #{} t/*unary-function*)
        nul (into #{} t/*nullary-function*)
        alt (fn [symb]
              (cond
               (bin symb) (rand-nth t/*binary-function*)
               (una symb) (rand-nth t/*unary-function*)
               (nul symb) (rand-nth t/*nullary-function*)))]
    (loop [i 0
           tr (z/down (z/seq-zip genome))
           changed false]
      (if (= size i)
        (if changed 
          (new-individual :genome (apply list (z/root tr)))
          ind)
        (if (< (rand) *mutation-rate*)
          (let [from (z/node tr)
                to (alt from)] 
            (recur (inc i) (z/next (z/next (z/replace tr to))) true))
          (recur (inc i) (z/next (z/next tr)) changed))))))

(defn crossover
  [ind1 ind2]
  (let [a (:genome ind1)
        b (:genome ind2)
        subtree (fn [tree index]
                  (nth (iterate #(z/next (z/next %))
                                (->> tree z/seq-zip z/down z/next))
                       index))
        a-size (-> a flatten count)
        b-size (-> b flatten count)
        a-subtr (subtree a (rand-int (dec a-size)))
        b-subtr (subtree b (rand-int (dec b-size)))]
    (cond
     (> a-size *max-genome-length*) (prn a)
     (> b-size *max-genome-length*) (prn b))
    (map #(new-individual :genome (apply list %))
         (filter #(<= (count (flatten %)) *max-genome-length*)
                 [(z/root (z/replace  a-subtr (z/node b-subtr)))
                  (z/root (z/replace  b-subtr (z/node a-subtr)))]))))
