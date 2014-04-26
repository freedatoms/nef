(ns nef.nes.es
  (:require [incanter 
             [core :as c]
             [charts :as ch]
             [datasets :as d]
             [io :as io]])
  (:use nef.neprotocol
        nef.utils
        nef.problem-domain
        nef.graphviz-enabled)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer]
           [nef.nes.es.java
            Evolution
            FitnessFunction
            ClassificationFitness
            RegressionFitness
            MazeFitness
            Logger]
           javax.swing.JFrame))

(defn- count* 
  [coll]
  (try  (count coll)
        (catch Exception e 1)))

(defn- run-custom-fitness
  [es run-params]
  (let [options (:options es)
        evaluator (fn [ind in]
                    (.evaluate ind (double-array in)))
        fitness (get-fitness-function
                 (:problem-domain es)
                 evaluator)
        do-with-best (get-do-with-best-individual-function
                      (:problem-domain es) evaluator)]
    (.evolve (Evolution. (int-array (:topology options))
                         (proxy [FitnessFunction] []
                           (countFitness [individual]
                             (fitness individual))))
             (int (:generations options 100))
             (int (:mu options 15))
             (int (:rho options 4))
             (int (:lambda options 450))
             (boolean (:commaSelection options false))
             (proxy [Logger] []
               (log [gen maxperf minperf avgperf
                     medianperf maxperf-since-gen0]
                 (send-off (:log es) conj 
                           {:generation gen,
                            :max-performance maxperf,
                            :min-performance minperf,
                            :mean-performance avgperf,
                            :median-performance medianperf,
                            :performance maxperf-since-gen0,
                            :max-performance-since-begining maxperf-since-gen0})))
             do-with-best)))

(defrecord ES
    [problem-domain
     options
     log]
  NeuroEvolution
  (run [this] 
    (run this {:file-prefix "/tmp/"}))
  (run [this run-params]
    (send (:log this) (fn [_] []))
    (run-custom-fitness this run-params)
    (send (:log this) c/to-dataset)
    (await (:log this)))

  (save-log-to-csv [this filename]
    (c/save (get-log this) filename))

  (show-log [this]
    (c/view @(:log this)))

  (plot [this]
    (plot this
          ["Generations" "Performance"] 
          [[:generation :performance]]))
  (plot [this [x-label y-label] xy-pairs]
    (let [[[xo yo] & xyp] xy-pairs]
      (loop [[[x y] & rest] xyp
             plot (ch/xy-plot (c/$ xo (get-log this))
                              (c/$ yo (get-log this))
                              :title (str (get-name problem-domain)
                                          " of " 
                                          (get-experiment-name problem-domain)
                                          " using ES")
                              :legend true
                              :x-label x-label
                              :y-label y-label
                              :series-label (humanize-keyword yo))]
        (if (and (not rest) (not x))
          (c/view plot)
          (recur rest (ch/add-lines plot 
                                    (c/$ x (get-log this))
                                    (c/$ y (get-log this))
                                    :series-label (humanize-keyword y)))))))

  (get-log [this]
    @(:log this)))

(defn make-es [problem-domain options]
  (ES. problem-domain options (agent [])))

(def dataset-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/Implementace/datasets/")

(defn make-iris-classification
  []
  (make-es (make-classification
            :experiment-name "Iris"
            :dataset (io/read-dataset (str dataset-prefix "iris.data"))
            :input-cols [0 1 2 3] 
            :output-col [4]) 
           {:topology [4 4 3]
            :mu 15, :rho 4, :lambda 450,
            :commaSelection false}))


(defn make-wine-classification
  []
  (make-es (make-classification
            :experiment-name "Wine"
            :dataset (io/read-dataset (str dataset-prefix "wine.data"))
            :input-cols (range 1 14)
            :output-col [0]
            :decrease-output true) 
           {:topology [13 3],
            :mu 10, :rho 4, :lambda 450,
            :commaSelection true,
            :decrease-outputs true}))


(defn make-glass-classification
  []
  (make-es (make-classification
            :experiment-name "Glass"
            :dataset (io/read-dataset (str dataset-prefix "glass.data"))
            :input-cols (range 1 10)
            :output-col [10]
            :decrease-output true) 
           {:topology [9 15 7],
            :mu 15, :rho 4, :lambda 450,
            :commaSelection true}))



(defn make-yacht-regression 
  []
  (make-es (make-regression
            :experiment-name "Yacht Hydrodynamics"
            :dataset (io/read-dataset
                      (str dataset-prefix "yacht_hydrodynamics.data")
                      :delim \space)
            :input-cols (range 0 6)
            :output-cols [6]
            :normalize-outputs true) 
           {:topology [6 1],
            :mu 10, :rho 4, :lambda 450,
            :commaSelection true}))

(defn make-maze-reinforcement-learning
  []
  (make-es (make-maze
            :normalize-inputs true)
           {:topology [5 8 6 3],
            :mu 10, :rho 4, :lambda 450,
            :commaSelection false}))



;; (def ccc (make-maze-reinforcement-learning))
;; (run ccc)
;; (show-log ccc)
;; (plot ccc)

