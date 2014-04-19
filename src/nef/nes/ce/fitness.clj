(ns nef.nes.ce.fitness
  (:require [nef.nes.ce
             [tree :as gpt]
             [neural-network :as nn]
             [cellular-encoding-j :as ce]])
  (:import nef.nes.ce.java.Cell
           [nef.nes.task.maze
            DiscreteMaze
            MoveAction])
  (:use nef.graphviz-enabled))

(def ^:dynamic *inputs* "Inputs for the neural net used by unary fitness function"
                                    [[0 0] [0 1] [1 0] [1 1]])

(def ^:dynamic *outputs* "Outputs for the neural net used by unary fitness function"
                                    [[0] [1] [1] [0]])



(defn xor-fitness [grammar inputs outputs]
  (let [etg (ce/evaluate-tree-grammar grammar)
        ;;net (p :ce->neural-net (ce/graph->neural-net etg))
        ;;sorted-nodes (p :top-sort (nn/topsort net))
        ]
    (+ (* 0.85 (/ (float (Math/abs
                          (reduce + (map (fn [i o]
                                           (if (= (take (count o)
                                                        (nn/evaluate-ff-net-cell etg i)) o)
                                             1
                                             -1)) inputs outputs))))
                  (count inputs)))
       (* 0.15 (+ (if (= (count (first inputs)) (count (.getOut ^Cell (first etg)))) 0.5 0)
                  (if (= (count (first outputs)) (count (.getIn ^Cell (second etg)))) 0.5 0))))))

(def ^:dynamic *fitness-function* "Fitness function for evolution"
  xor-fitness)


(defn max-pos[v]
  (loop [[x & xs] v
         max      0
         ith      0
         i        0]
    (if x
      (if (> x max)
        (recur xs x  i    (inc i))
        (recur xs max ith (inc i)))
      ith)))

;; (defn classification-fitness-recognized-patterns
;;   "Fitness used for classification"
;;   ([grammar] (classification-fitness-recognized-patterns grammar *inputs* *outputs*))
;;   ([grammar inputs outputs]
;;      (let [etg (ce/evaluate-tree-grammar grammar)]
;;        (float (/ (reduce + (mapv (fn [in out]
;;                                    (if (== (max-pos (nn/evaluate-ff-net-cell etg in)) (max-pos out))
;;                                      1
;;                                      0))
;;                                  inputs
;;                                  outputs)) (count outputs))))))

;; (defn avg-error
;;   ([grammar] (avg-error grammar *inputs* *outputs*))
;;   ([grammar inputs outputs]
;;      (let [etg (ce/evaluate-tree-grammar grammar)]
;;        (float (/ (reduce + (mapv (fn [in out]
;;                                    (reduce + (mapv (fn [i o]
;;                                                      (Math/tanh (Math/abs (- o i)))) (nn/evaluate-ff-net-cell etg in) out)))
;;                                  inputs
;;                                  outputs)) (* 10 (count outputs)
;;                                               (count (first outputs))))))))

;; (defn classification-fitness2
;; "Fitness used for classification"
;; ([grammar] (classification-fitness2 grammar *inputs* *outputs*))
;; ([grammar inputs outputs]
;;    (let [etg (ce/evaluate-tree-grammar grammar)]
;;      (-
;;       (classification-fitness-recognized-patterns grammar inputs outputs)
;;       (avg-error grammar inputs outputs)
;;       ))))

(defn classification-fitness
  "Fitness used for classification"
  ([grammar] (classification-fitness grammar *inputs* *outputs*))
  ([grammar inputs outputs]
     (let [etg (ce/evaluate-tree-grammar grammar)]
       (float (/ (reduce + (mapv (fn [in out]
                                   (if (== (max-pos (nn/evaluate-ff-net-cell etg in)) out)
                                     1
                                     0))
                                 inputs
                                 outputs)) (count outputs))))))


(defn regression-fitness
  "fitness used for regression of 1 variable"
  ([grammar] (regression-fitness grammar *inputs* *outputs*))
  ([grammar inputs outputs]
     (let [etg (ce/evaluate-tree-grammar grammar)]
       (float (/ (reduce + (mapv (fn [in out]
                                   (- 1 (Math/pow (- out (first (nn/evaluate-ff-net-cell etg in))) 2)))
                                 inputs
                                 outputs)) (count outputs))))))

(defn regression-fitness2
  "fitness used for regression of 1 variable"
  ([grammar] (regression-fitness grammar *inputs* *outputs*))
  ([grammar inputs outputs]
     (let [etg (ce/evaluate-tree-grammar grammar)]
       (float (/ (reduce + (mapv (fn [in out]
                                   (- 1 (Math/abs (- out (first (nn/evaluate-ff-net-cell etg in))))))
                                 inputs
                                 outputs)) (count outputs))))))



(defn maze-fitness
  "fitness used for maze"
  ([grammar] (maze-fitness grammar *inputs* *outputs*))
  ([grammar _ _]
     (let [etg (ce/evaluate-tree-grammar grammar)
           m (DiscreteMaze.)
           a (.getNewActor m)
           s (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
           not-a-wall (fn [c] (if (= c \w) 0.0 1.0))
           steps (loop [i 0]
                   (if (< i(* 10 s))
                     (if-not (.move m a  (case (max-pos (nn/evaluate-ff-net-cell etg [i
                                                                                       (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
                                                                                       (not-a-wall (.lookForward m a))
                                                                                       (not-a-wall (.lookLeft m a))
                                                                                       (not-a-wall (.lookRight m a))]))
                                             1 MoveAction/TURN_LEFT
                                             2 MoveAction/TURN_RIGHT
                                             MoveAction/FORWARD))
                       (recur (inc i))
                       i)
                     i))]
       (float (+ 100 (if (.isTarget m a)
                       (/ s steps)
                       (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a)))))))))

(defn fitness
  "Fitness function takes grammar, inputs and outputs.
If inputs and outputs are not provided dynamically bound
variables *inputs*, *outputs* will be used."
  ([grammar]
     (fitness grammar *inputs* *outputs*))
  ([grammar inputs outputs]
     (*fitness-function* grammar inputs outputs)))



