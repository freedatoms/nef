(ns nef.nes.neat.operators
  (:require [nef.nes.neat
             [gene :as gene]
             [genome :as genome]]
            )
  (:use [nef.nes.neat evolution-parameters])
  (:import [java.util Random]))

(defn- add-node
  "Takes genome and replace existing connection with node and two connections."
  [^nef.nes.neat.genome.Genome g]
  (let [len  (count (:connection-genes g))
        ng   (inc (count (:node-genes g)))
        gene (loop [cg (rand-int len)
                    cnt 0]
               (if (:enabled? (nth (:connection-genes g) cg))
                 cg
                 (if (> cnt len)
                   nil
                   (recur (rand-int len) (inc cnt)))))]
    (if-not gene
      g
      (let [cg   (nth (:connection-genes g) gene)
            node-genes (conj (:node-genes g) (gene/->Node-gene ng :hidden))
            conn-genes (assoc-in (:connection-genes g) [gene :enabled?] false)
            c1 (gene/->Connection-gene (:in cg) ng 1.0 true (or (@gene-pool [(:in cg) ng])
                                                                (swap! innovation-number inc)))
            c2 (gene/->Connection-gene ng (:out cg) (:weight cg) true (or (@gene-pool [ng (:out cg)])
                                                                          (swap! innovation-number inc)))]
        (swap! gene-pool assoc
               [(:in c1) (:out c1)] (:innov c1)
               [(:in c2) (:out c2)] (:innov c2))
        (genome/->Genome node-genes
                         (vec (sort-by :innov (into conn-genes [c1 c2]))))))))

(defn- new-conn
  "Creates new tuple first item is from ins second from outs."
  [ins outs]
  (let [in (rand-nth ins)
        out (rand-nth outs)]
    [in out]))

(defn- add-connection
  "Takes genome and adds one connection gene."
  [^nef.nes.neat.genome.Genome g]
  (let [in      (count (filter #{:input :bias} (map :type (:node-genes g))))
        out     (count (filter #{:output} (map :type (:node-genes g))))
        total   (count (:node-genes g))
        ins     (into (vec (range 1 (inc in))) (range (+ in out 1) (inc total)))
        outs    (vec (range (inc in) (inc total)))
        conns   (set (map #(vector (:in %) (:out %)) (:connection-genes g)))
        nc      (loop [conn (new-conn ins outs)
                       cnt  0]
                  (if (conns conn)
                    (if (< cnt (* total total))
                      (recur (new-conn ins outs) (inc cnt))
                      nil)
                    conn))]
    (if-not nc
      g
      (let [node-genes (:node-genes g)
            c (gene/->Connection-gene (first nc) (second nc) (rand-weight) true (or (@gene-pool nc)
                                                                                    (swap! innovation-number inc)))
            conn-genes (conj (:connection-genes g) c)]
        (swap! gene-pool assoc nc (:innov c))
        (genome/->Genome node-genes (vec (sort-by :innov conn-genes)))))))

(defn- clamp-weight
  [weight]
  (if (== @clamp-weight-factor 0.0)
    weight
    (let [[lo hi] (mapv (partial * @clamp-weight-factor) @weight-range)]
      (cond
       (< weight lo) lo
       (> weight hi) hi
       :else weight))))

(defn- mutate-weights
  "With probability of @mutate-weights-perturb-prob uniformly perturbs weight
   otherwise replaces weight random number from uniform distribution."
  [^nef.nes.neat.genome.Genome g]
  (let [rnd (Random.)
        randf (fn [w] (if (< (rand) @mutate-weights-perturb-prob)
                       (clamp-weight (+ (* (.nextGaussian rnd) @mutate-weights-perturb-sigma) w))
                       (rand-weight)))]
    (genome/->Genome (:node-genes g) (mapv #(update-in  % [:weight] randf) (:connection-genes g)))))

(defn- prob-call
  "Call (f arg) with probability prob otherwise returs arg."
  [prob f arg]
  (if (< (rand) prob)
    (f arg)
    arg))

(defn mutation
  "Mutate genome g.
   Mutates weight with probability of @mutate-weights-prob.
   Adds connection with probability of @add-connection-prob.
   Adds node with probability of @add-node-prob."
  [^nef.nes.neat.genome.Genome g]
  (if (< (rand) @add-node-prob)
    (add-node g)
    (if (< (rand) @add-connection-prob)
      (add-connection g)
      (if (< (rand) @mutate-weights-prob)
        (mutate-weights g)
        g))))

(defn- merge-genes
  [[x y :as arg]]
  (let [gene (if (< (rand) @mate-by-choosing-prob)
               (rand-nth arg)
               (assoc x :weight (/ (+ (:weight x)
                                      (:weight y)) 2)))]
    (if (not (and (:enabled? x)
                  (:enabled? y))) 
      (assoc gene :enabled?  (< (rand) @disable-in-crossover))
      gene)))

(defn crossover
  "Takes two genomes and difference of fitnesses"
  [^nef.nes.neat.genome.Genome g1 ^nef.nes.neat.genome.Genome g2 f1-f2]
  (let [node-genes (cond
                    (> f1-f2 0) (:node-genes g1)
                    (< f1-f2 0) (:node-genes g2)
                    :else (if (> (count (:node-genes g1))
                                 (count (:node-genes g2)))
                            (:node-genes g1)
                            (:node-genes g2)))
        mg (genome/match-genes g1 g2)]
    (genome/->Genome node-genes  (vec (filter identity
                                              (cond
                                               (> f1-f2 0) (mapv (fn [[x y :as arg]]
                                                                   (cond
                                                                    (and x y) (merge-genes arg)
                                                                    x x
                                                                    :else nil)) mg)
                                               (< f1-f2 0) (mapv (fn [[x y :as arg]]
                                                                   (cond
                                                                    (and x y) (merge-genes arg)
                                                                    y y
                                                                    :else nil)) mg)
                                               :else (mapv (fn [[x y :as arg]]
                                                             (if (and x y)
                                                               (merge-genes arg)
                                                               (or x y))) mg)))))))


