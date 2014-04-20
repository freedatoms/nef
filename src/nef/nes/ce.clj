(ns nef.nes.ce
  (:require [incanter 
             [core :as c]
             [charts :as ch]
             [datasets :as d]
             [io :as io]]
            [nef.nes.ce
             [cellular-encoding-j :as ce]
             [neural-network :as nn]
             [evolution :refer [simple-evolution]]
             [individual :as i]
             [tree :as t]
             [fitness :as f]])
  (:use nef.neprotocol
        nef.graphviz-enabled)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer]
           javax.swing.JFrame))


(defn- run-custom-fitness* [ce run-params fitness input output]
  (let [options (:ce-options ce)
        filename (str (:file-prefix run-params) "ce-" (:name (:problem-domain ce))
                      "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
    (binding [f/*inputs* input
              f/*outputs* output
              ce/*default-activation-function* (:default-activation-function options ce/act-tanh)
              ce/*lifespan* (:lifespan options 1)
              #_(*graphviz-dpi* (:graphviz-dpi options 50))
              i/*genome-length* (:initial-genome-length options 10)
              i/*max-genome-length* (:max-genome-length options 50)
              f/*fitness-function* fitness
              t/*binary-function* (:binary-functions options ['seq 'par])
              t/*unary-function* (:unary-functions options ['addbias])
              t/*nullary-function* (:nullary-functions options ['end])]
      (simple-evolution (:population-size options 450)
                        (:generation-count options 100)
                        :log (:log ce)
                        :best-individual-function
                        (fn [ind gen]
                          (when (:save-images run-params)
                            (let [net (ce/evaluate-tree-grammar (:genome ind))
                                  fn (format filename gen (:fitness ind))]
                              (save-dot (:genome ind) (str fn "-grammar.dot"))
                              (save-image (:genome ind) (str fn "-grammar.png"))
                              (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                              (save-image (ce/graph->neural-net net) (str fn "-net.png")))))))))

(defn- run-classification
  [ce run-params]
  (let [dataset (c/to-matrix (:data (:problem-domain ce)))
        input (c/to-vect  (c/sel dataset :cols (:input-cols (:problem-domain ce))))
        output  (let [outputs (c/to-vect (c/sel dataset :cols
                                                 (:output-cols (:problem-domain es))))]
                   (if (:decrease-outputs (:problem-domain es))
                     (mapv dec outputs)
                     outputs))]
    (run-custom-fitness* ce run-params  f/classification-fitness input output)))

(defn- run-regression
  [ce run-params]
  (let [dataset (c/to-matrix (:data (:problem-domain ce)))
        input   (c/to-vect  (c/sel dataset :cols (:input-cols (:problem-domain ce))))
        output  (let [data (c/to-vect (c/sel dataset :cols 
                                             (:output-cols (:problem-domain ce))))]
                  (if (seq? (first data))
                    (mapv min-max-normalize-column data)
                    (min-max-normalize-column data)))]
    (run-custom-fitness* ce run-params  f/regression-fitness2 input output)))

(defn- run-custom-fitness
  [ce run-params]
  (run-custom-fitness* 
   ce
   run-params
   ((:fitness-function run-params)
    nn/evaluate-ff-net-cell)))

(defn- run-maze
  [ce run-params]
  (let [options (:ce-options ce)]
    (binding [ce/*default-activation-function* (:default-activation-function options ce/act-tanh)
              ce/*lifespan* (:lifespan options 1)
              i/*genome-length* (:initial-genome-length options 10)
              i/*max-genome-length* (:max-genome-length options 50)
              f/*fitness-function* f/maze-fitness
              t/*binary-function* (:binary-functions options ['seq 'par])
              t/*unary-function* (:unary-functions options ['addbias])
              t/*nullary-function* (:nullary-functions options ['end])]
      (let [fr (JFrame.)
            filename (str (:file-prefix run-params) "ce-" (:name (:problem-domain ce))
                          "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
        (.setSize fr
                  (:maze-size run-params 600)
                  (:maze-size run-params 600))
        (simple-evolution (:population-size options 450)
                          (:generation-count options 100)
                          :log (:log ce)
                          :best-individual-function
                          (fn [ind gen]
                            (let [m (DiscreteMaze.)
                                  net (ce/evaluate-tree-grammar (:genome ind))
                                  mv (DiscreteMazeViewer. m   (fn [in] (double-array (nn/evaluate-ff-net-cell net in))))
                                  fn (format filename gen (:fitness ind))]
                              (.setTitle fr (str "Maze fitness: " (:fitness ind)))
                              (.removeAll (.getContentPane fr))
                              (.add (.getContentPane fr) mv)
                              (when (not (.isShowing fr))
                                (.setVisible fr true))
                              (when (:save-images run-params)
                                (save-dot (:genome ind) (str fn "-grammar.dot"))
                                (save-image (:genome ind) (str fn "-grammar.png"))
                                (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                (save-image (ce/graph->neural-net net) (str fn "-net.png"))
                                (.saveImage mv (str fn "-maze.png") 800 800))
                              (.revalidate fr))))))))


(defrecord CE
    [problem-domain
     ce-options
     log]
  NeuroEvolution
  (run [this] 
    (run this {:show-nets true, :save-images false, 
               :file-prefix "/tmp/"}))
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
                                          " using CE")
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

(defn make-ce [problem-domain options]
  (CE. problem-domain options (agent [])))

(def dataset-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/Implementace/datasets/")

(defn make-iris-classification
  []
  (make-ce (make-classification-of "Iris"
                                   (io/read-dataset (str dataset-prefix "iris.data"))
                                   [0 1 2 3] 
                                   [4]) 
           {:unary-functions ['halfbias 'doublebias 'addbias 'subbias
                              'addval 'subval  'halfval 'doubleval
                              'inclr 'declr 'acttanh 'actsigmoid
                              'actsgn 'actsin 'actgauss]}))
(defn make-wine-classification
  []
  (make-ce (make-classification-of "Wine"
                                   (io/read-dataset (str dataset-prefix "wine.data")) 
                                   (range 1 14)
                                   [0]
                                   :decrease-outputs true) 
           {:unary-functions ['halfbias 'doublebias 'addbias 'subbias
                              'addval 'subval  'halfval 'doubleval
                              'inclr 'declr 'wait 'cut 'acttanh
                              'actsigmoid 'actsgn 'actsin 'actgauss]}))
(defn make-glass-classification
  []
  (make-ce (make-classification-of "Glass" 
                                   (io/read-dataset (str dataset-prefix "glass.data"))
                                   (range 1 10)
                                   [10]
                                   :decrease-outputs true) 
           {:unary-functions  ['addbias 'subbias 'mult10bias 'mult-1bias
                               'mult0.1bias 'addval 'subval 'mult10val
                               'mult-1val 'mult0.1val 'inclr 'declr
                               'acttanh 'actsigmoid 'actsgn 'actsin
                               'actgauss],
            :max-genome-length 100}))



(defn make-yacht-regression 
  []
  (make-ce (make-regression-of "Yacht Hydrodynamics"
                               (io/read-dataset (str dataset-prefix 
                                                     "yacht_hydrodynamics.data")
                                                :delim \space)
                               (range 0 6)
                               [6])
           {:unary-functions ['addbias 'subbias 'mult10bias 'mult-1bias
                              'mult0.1bias 'addval 'subval 'mult10val 
                              'mult-1val 'mult0.1val 'inclr 'declr
                              'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss]}))

(defn make-maze-reinforcement-learning
  []
  (make-ce (make-maze)
           {:unary-functions ['addbias 'subbias 'mult10bias 'mult-1bias 
                              'mult0.1bias 'addval 'subval 'mult10val 
                              'mult-1val 'mult0.1val 'inclr 'declr 
                              'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss]}))


;; (def ccc (make-maze-reinforcement-learning))
;; (run ccc)
;; (show-log ccc)
;; (plot ccc)
