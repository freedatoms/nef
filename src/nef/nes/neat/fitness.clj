(ns nef.nes.neat.fitness
  (:require [nef.nes.neat 
             [evolution :as evo]
             [evolution-parameters :as ep]
             [gui :as gui]
             [neural-net :as net]]
            [incanter
             [core :as co]
             [io :as io]                
             [datasets :as dat]])
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer
            MoveAction]
           javax.swing.JFrame))

(defn maxpos 
  [coll]
  (loop [[x & xs] coll
         max (first coll)
         maxI 0
         i 0]
    (if x
      (if (> x max)
        (recur xs x i (inc i))
        (recur xs max maxI (inc i)))
      maxI)))

(defn make-classification-fitness
  [inputs outputs]
  (fn [genome]
    (let [evals (mapv #(net/evaluate-neural-net-with-activation-cycles genome % 100) inputs)
          succ (mapv #(= (maxpos %1) %2) evals outputs)]
      {:solved (reduce #(and %1 %2) succ)
       :success-rate (/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
                        (count succ))
       :fitness (double (Math/pow (/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
                                     (count succ)) 2))})))

(defn make-regression-fitness
  [input output success-threshold]
  (fn [genome]
    (let [evals (mapv #(net/evaluate-neural-net-with-activation-cycles genome % 100) input)
          succ (mapv (fn [e o] (reduce #(and %1 %2) true
                                       (mapv #(< (Math/abs ^float (- %1 %2)) success-threshold) e o)))
                     evals output)]
      {:fitness (double (max 0.000000001 
                             (- 100 (/ (reduce + 
                                               (mapv (fn [evaluated out] 
                                                       (reduce #(+ %1 (* %2 %2)) 0 
                                                               (mapv #(- %1 %2) evaluated out)))
                                                     evals output))
                                       (count evals)))))
       

       :solved (reduce #(and %1 %2) succ)
       :success-rate (- 1 (/ (reduce + 
                                     (mapv (fn [evaluated out] 
                                             (reduce #(+ %1 (Math/abs ^float %2)) 0 
                                                     (mapv #(- %1 %2) evaluated out)))
                                           evals output))
                             (count evals)))})

    (defn maze-fitness
      "fitness used for maze"
      [genome]
      (let [m (DiscreteMaze.)
            a (.getNewActor m)
            s (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
            not-a-wall (fn [c] (if (= c \w) 0.0 1.0))
            steps (loop [i 0]
                    (if (< i(* 10 s))
                      (if (.move m a  
                                 (case (maxpos (net/evaluate-neural-net-with-activation-cycles
                                                genome
                                                [(double i)
                                                 (double (.shortestPathToTarget m ^int (.getX a) ^int (.getY a)))
                                                 (not-a-wall (.lookForward m a))
                                                 (not-a-wall (.lookLeft m a))
                                                 (not-a-wall (.lookRight m a))]
                                                100))
                                   1 MoveAction/TURN_LEFT
                                   2 MoveAction/TURN_RIGHT
                                   MoveAction/FORWARD))
                        i
                        (recur (inc i)))
                      i))]
        {:fitness (float (+ 100 (if (.isTarget m a)
                                  (/ s steps)
                                  (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))
         :solved (.isTarget m a)
         :success-rate (float (+ 100 (if (.isTarget m a)
                                       (/ s steps)
                                       (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))}))

    (defn maze-fitness-using-normalized-inputs
      "fitness used for maze"
      [genome]
      (let [m (DiscreteMaze.)
            a (.getNewActor m)
            s (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
            not-a-wall (fn [c] (if (= c \w) 0.0 1.0))
            steps (loop [i 0]
                    (if (< i(* 10 s))
                      (if (.move m a  
                                 (case (maxpos (net/evaluate-neural-net-with-activation-cycles
                                                genome
                                                [(double (/ i (* 10 s)))
                                                 (double (/ (.shortestPathToTarget m ^int (.getX a) ^int (.getY a)) 
                                                            s))
                                                 (not-a-wall (.lookForward m a))
                                                 (not-a-wall (.lookLeft m a))
                                                 (not-a-wall (.lookRight m a))]
                                                100))
                                   1 MoveAction/TURN_LEFT
                                   2 MoveAction/TURN_RIGHT
                                   MoveAction/FORWARD))
                        i
                        (recur (inc i)))
                      i))]
        {:fitness (float (+ 100 (if (.isTarget m a)
                                  (/ s steps)
                                  (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))
         :solved (.isTarget m a)
         :success-rate (float (+ 100 (if (.isTarget m a)
                                       (/ s steps)
                                       (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))}))))
