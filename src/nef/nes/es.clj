(ns nef.nes.es
  (:require [incanter 
             [core :as c]
             [charts :as ch]
             [datasets :as d]
             [io :as io]])
  (:use nef.neprotocol
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

(defn- run-class-or-reg 
  [es run-params fitness input output]
  (let [options (:options es)]
    (.evolve (Evolution. (int-array (:topology options (int-array [(count* (first input))
                                                                   (count* (first output))])))
                         fitness)
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
                            :max-performance-since-begining maxperf-since-gen0}))))))

(defn- run-classification
  [es run-params]
  (let [dataset (c/to-matrix (:data (:problem-domain es)))
        input (into-array (map double-array 
                               (c/to-vect  (c/sel dataset :cols 
                                                  (:input-cols (:problem-domain es))))))
        output  (int-array 
                 (let [outputs (c/to-vect (c/sel dataset :cols
                                                 (:output-cols (:problem-domain es))))]
                   (if (:decrease-outputs (:problem-domain es))
                     (mapv dec outputs)
                     outputs)))]
    (run-class-or-reg es run-params (ClassificationFitness. input output) input output)))

 (defn- run-regression
   [es run-params]
   (let [dataset (c/to-matrix (:data (:problem-domain es)))
        input (into-array (map double-array 
                               (c/to-vect  (c/sel dataset :cols 
                                                  (:input-cols (:problem-domain es))))))
        output  (int-array
                 (c/to-vect (c/sel dataset :cols (:output-cols (:problem-domain es)))))]
    (run-class-or-reg es run-params (RegressionFitness. input output) input output)))

 (defn- run-custom-fitness
   [ce run-params]
   (proxy [FitnessFunction] []
     (countFitness [individual]
       ((:fitness-function run-params)
        #(.evaluate %1 %2)))))

 (defn- run-maze
   [ce run-params]
;;   (let [options (:ce-options ce)]
;;     (binding [ce/*default-activation-function* (:default-activation-function options ce/act-tanh)
;;               ce/*lifespan* (:lifespan options 1)
;;               i/*genome-length* (:initial-genome-length options 10)
;;               i/*max-genome-length* (:max-genome-length options 50)
;;               f/*fitness-function* f/maze-fitness
;;               t/*binary-function* (:binary-functions options ['seq 'par])
;;               t/*unary-function* (:unary-functions options ['addbias])
;;               t/*nullary-function* (:nullary-functions options ['end])]
;;       (let [fr (JFrame.)
;;             filename (str (:file-prefix run-params) "ce-" (:name (:problem-domain ce))
;;                           "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
;;         (.setSize fr
;;                   (:maze-size run-params 600)
;;                   (:maze-size run-params 600))
;;         (simple-evolution (:population-size options 450)
;;                           (:generation-count options 100)
;;                           :log (:log ce)
;;                           :best-individual-function
;;                           (fn [ind gen]
;;                             (let [m (DiscreteMaze.)
;;                                   net (ce/evaluate-tree-grammar (:genome ind))
;;                                   mv (DiscreteMazeViewer. m   (fn [in] (double-array (nn/evaluate-ff-net-cell net in))))
;;                                   fn (format filename gen (:fitness ind))]
;;                               (.setTitle fr (str "Maze fitness: " (:fitness ind)))
;;                               (.removeAll (.getContentPane fr))
;;                               (.add (.getContentPane fr) mv)
;;                               (when (not (.isShowing fr))
;;                                 (.setVisible fr true))
;;                               (when (:save-images run-params)
;;                                 (save-dot (:genome ind) (str fn "-grammar.dot"))
;;                                 (save-image (:genome ind) (str fn "-grammar.png"))
;;                                 (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
;;                                 (save-image (ce/graph->neural-net net) (str fn "-net.png"))
;;                                 (.saveImage mv (str fn "-maze.png") 800 800))
;;                               (.revalidate fr))))))))
)

(defrecord ES
    [problem-domain
     options
     log]
  NeuroEvolution
  (run [this] 
    (run this {:file-prefix "/tmp/"}))
  (run [this run-params]
    (send (:log this) (fn [_] []))
    (case (:type (:problem-domain this))
      :classification (run-classification this run-params)
      :regression (run-regression this run-params)
      :maze (run-maze this run-params)
      :custom-fitness (run-custom-fitness this run-params))
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
                              :title (str (humanize-keyword (:type problem-domain))
                                          " of " 
                                          (:name problem-domain)
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
  (make-es (make-classification-of "Iris"
                                   (io/read-dataset (str dataset-prefix "iris.data"))
                                   [0 1 2 3] 
                                   [4]) 
           {:topology [4 4 3]
            :mu 15, :rho 4, :lambda 450,
            :commaSelection false}))


(defn make-wine-classification
  []
  (make-es (make-classification-of "Wine"
                                   (io/read-dataset (str dataset-prefix "wine.data"))
                                   (range 1 14)
                                   [0]
                                   :decrease-outputs true) 
           {:topology [13 3],
            :mu 10, :rho 4, :lambda 450,
            :commaSelection true,
            :decrease-outputs true}))


(defn make-glass-classification
  []
  (make-es (make-classification-of "Glass"
                                   (io/read-dataset (str dataset-prefix "glass.data"))
                                   (range 1 10)
                                   [10]
                                   :decrease-outputs true) 
           {:topology [9 15 7],
            :mu 15, :rho 4, :lambda 450,
            :commaSelection true}))



 (defn make-yacht-regression 
   []
)

(defn make-maze-reinforcement-learning
  []
  )


(def ccc (make-glass-classification))
(run ccc)
(show-log ccc)
(plot ccc ["Gens" "Perf"]
      [[:generation :performance] 
       [:generation :mean-performance]
       [:generation :median-performance]
       [:generation :max-performance]])
