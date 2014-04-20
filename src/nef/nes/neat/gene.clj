(ns nef.nes.neat.gene)

(defrecord Node-gene
    [^int id
     type
     ])

(defrecord Connection-gene
    [^int     in
     ^int     out
     ^double  weight
     ^boolean enabled?
     ^int     innov
     ])

(defn equal-but-innov?
  [^Connection-gene g1 ^Connection-gene g2]
  (and (= (:in g1) (:in g2))
       (= (:out g1) (:out g2))
       (= (:weight g1) (:weight g2))
       (= (:enabled? g1) (:enabled? g2))))

