(ns nef.problem-domain
  (:require [incanter 
             [core :as c]
             [charts :as ch]
             [datasets :as d]
             [io :as io]])
  (:use nef.utils
        nef.preprocessing)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer
            MoveAction]
           javax.swing.JFrame))

(defprotocol ProblemDomain
  (get-fitness-function [this evaluator] 
    "Returns fitness function that takes [individual] 
     and evaluates the individual using evaluator.
     Evaluator takes [individual input].")
  (get-do-with-best-individual-function [this evaluator] 
    "Can be used for showing maze etc.")
  (get-name [this] 
    "Name of problem domain")
  (get-experiment-name [this]
    "Name of experiment"))


(defrecord Classification
    [experiment-name 
     dataset
     input-cols
     output-col
     normalize-inputs
     decrease-output]
  ProblemDomain
  (get-fitness-function [this evaluator]
    (let [data (c/to-matrix (:dataset this))
          inputs  (->> (c/to-vect  (c/sel data :cols (:input-cols this)))
                      (?-> (:normalize-inputs this)
                           (min-max-normalize-columns [-1 1])))
          outputs (->> (c/to-vect (c/sel data :cols (:output-col this)))
                      (?-> (:decrease-output this) #(mapv dec %)))]
      (fn [individual]
        (double (/ (reduce + 
                           (mapv (fn [in out]
                                   (if (== (max-pos (evaluator individual in)) out)
                                     1
                                     0))
                                 inputs
                                 outputs)) 
                   (count outputs))))))
  (get-do-with-best-individual-function [this evaluator]
    (fn [best-individual generation fitness]
      nil))
  (get-name [this]
    "Classification")
  (get-experiment-name [this]
    (:experiment-name this)))

(defn make-classification
  [&{:keys [experiment-name dataset input-cols output-col
           normalize-inputs decrease-output]}]
  (if-not (and experiment-name dataset input-cols output-col)
    (throw (Error. "Arguments experiment-name, dataset, input-cols, output-col must be non-null.")))
  (Classification. experiment-name dataset input-cols output-col
                   normalize-inputs decrease-output))

(defrecord Regression
    [experiment-name 
     dataset
     input-cols
     output-cols
     normalize-inputs
     normalize-outputs]
  ProblemDomain
  (get-fitness-function [this evaluator]
    (let [data (c/to-matrix (:dataset this))
          inputs  (->> (c/to-vect  (c/sel data :cols (:input-cols this)))
                      (?-> (:normalize-inputs this)
                           (min-max-normalize-columns [-1 1])))
          outputs (->> (mapv vector (c/to-vect  (c/sel data :cols (:output-cols this))))
                      (?-> (:normalize-outputs this)
                           (min-max-normalize-columns [0 1])))] ;; Neat uses sigmoid with image range [0,1]
      (fn [individual]
        (double (/ (reduce + 
                           (mapv (fn [in out]
                                   (double (- 1 (/ (pair-wise-reduce 
                                                    (fn [acc x y]
                                                      (+ acc (Math/abs (- x y))))
                                                    0  out  (evaluator individual in))
                                                   (count out)))))
                                 inputs
                                 outputs))
                   (count outputs))))))
  (get-do-with-best-individual-function [this evaluator]
    (fn [best-individual generation fitness]
      nil))
  (get-name [this]
    "Regression")
  (get-experiment-name [this]
    (:experiment-name this)))

(defn make-regression
  [&{:keys [experiment-name dataset input-cols output-cols
           normalize-inputs normalize-outputs]}]
  (if-not (and experiment-name dataset input-cols output-cols)
    (throw (Error. "Arguments experiment-name, dataset, input-cols, output-col must be non-null.")))
  (Regression. experiment-name dataset input-cols output-cols
               normalize-inputs normalize-outputs))


(defrecord Maze
    [normalize-inputs
     options]
  ProblemDomain
  (get-fitness-function [this evaluator]
    (fn [individual]
      (let [m (DiscreteMaze.)
            a (.getNewActor m)
            s (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
            not-a-wall (fn [c] (if (= c \w) 0.0 1.0))
            steps (loop [i 0]
                    (if (< i(* 10 s))
                      (if-not (.move m a  
                                     (case (max-pos 
                                            (evaluator 
                                             individual 
                                             [(?-> (:normalize-inputs this)
                                                   #(double (/ % (* 10 s))) i) 
                                              (?->  (:normalize-inputs this)
                                                    #(double (/ % s))  
                                                    (.shortestPathToTarget 
                                                     m
                                                     ^int (.getX a) ^int (.getY a)))
                                              (not-a-wall (.lookForward m a))
                                              (not-a-wall (.lookLeft m a))
                                              (not-a-wall (.lookRight m a))]))
                                       1 MoveAction/TURN_LEFT
                                       2 MoveAction/TURN_RIGHT
                                       MoveAction/FORWARD))
                        (recur (inc i))
                        i)
                      i))]
        (double (+ 100 (if (.isTarget m a)
                         (/ s steps)
                         (- (.shortestPathToTarget 
                             m ^int (.getX a) ^int (.getY a)))))))))

  (get-do-with-best-individual-function [this evaluator]
    (let [fr (JFrame.)]
      (.setSize fr 600 600)
      (fn [best-individual generation fitness]
        (let [m (DiscreteMaze.)
              mv (DiscreteMazeViewer. m  (fn [in] (double-array (evaluator best-individual in)))
                                      (boolean (:normalize-inputs this)))]
          (.setTitle fr (str "Maze fitness: " fitness))
          (.removeAll (.getContentPane fr))
          (.add (.getContentPane fr) mv)
          (when (not (.isShowing fr))
            (.setVisible fr true))
          (when (:save-images options)
            (.saveImage mv (format (:filename-format options) generation fitness) 800 800))
          (.revalidate fr)))))

  (get-name [this]
    "Reinforcement Learning")

  (get-experiment-name [this]
    "Maze"))


(defn make-maze
  [&{:keys [normalize-inputs options]}]
  (Maze. normalize-inputs options))
