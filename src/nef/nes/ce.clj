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
             [tree :as t]])
  (:use nef.neprotocol
        nef.problem-domain
        nef.utils
        nef.graphviz-enabled)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer]
           javax.swing.JFrame))


(defn- run-custom-fitness* [ce run-params]
  (let [options (:options ce)
        filename (str (:file-prefix run-params) "ce-" (get-name (:problem-domain ce))
                      "-" (get-experiment-name (:problem-domain ce))
                      "-gen-%d-succ-%f" " (" (java.util.Date.) ")")
        evaluator (fn [ind in]
                    (nn/evaluate-ff-net-cell
                     (ce/evaluate-tree-grammar ind)
                     in))
        do-with-best (get-do-with-best-individual-function (:problem-domain ce)
                                                           evaluator)]
    (binding [ce/*default-activation-function* (:default-activation-function options ce/act-tanh)
              ce/*lifespan* (:lifespan options 1)
              *graphviz-dpi* (:graphviz-dpi run-params 50)
              i/*genome-length* (:initial-genome-length options 10)
              i/*max-genome-length* (:max-genome-length options 50)
              i/*fitness-function* (get-fitness-function
                                    (:problem-domain ce)
                                    evaluator)
              t/*binary-function* (:binary-functions options ['seq 'par])
              t/*unary-function* (:unary-functions options ['addbias])
              t/*nullary-function* (:nullary-functions options ['end])]
      (simple-evolution (:population-size options 450)
                        (:generation-count options 100)
                        :log (:log ce)
                        :best-individual-function
                        (fn [ind gen]
                          (do-with-best (:genome ind) gen  (:fitness ind))
                          (when (:save-images run-params)
                            (let [net (ce/evaluate-tree-grammar (:genome ind))
                                  fn (format filename gen (:fitness ind))]
                              (save-dot (:genome ind) (str fn "-grammar.dot"))
                              (save-image (:genome ind) (str fn "-grammar.png"))
                              (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                              (save-image (ce/graph->neural-net net) (str fn "-net.png")))))))))

(defrecord CE
    [problem-domain
     options
     log]
  NeuroEvolution
  (run [this] 
    (run this {:show-nets true, :save-images false, 
               :file-prefix "/tmp/"}))
  (run [this run-params]
    (send (:log this) (fn [_] []))
    (run-custom-fitness* this run-params)
    (send (:log this) c/to-dataset)
    (await (:log this)))

  (save-log-to-csv [this filename]
    (try
      (c/save (get-log this) filename)
      (catch Exception e
        (println e))))

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
                              :title (str (get-name (:problem-domain this))
                                          " of " 
                                          (get-experiment-name (:problem-domain this))
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

(defn make-iris-classification
  []
  (make-ce (make-classification
            :experiment-name  "Iris"
            :dataset (io/read-dataset (str @dataset-prefix "iris.data"))
            :input-cols [0 1 2 3] 
            :output-col [4]) 
           {:unary-functions ['halfbias 'doublebias 'addbias 'subbias
                              'addval 'subval  'halfval 'doubleval
                              'inclr 'declr 'acttanh 'actsigmoid
                              'actsgn 'actsin 'actgauss]}))

(defn make-wine-classification
  []
  (make-ce (make-classification
            :experiment-name "Wine"
            :dataset (io/read-dataset (str @dataset-prefix "wine.data")) 
            :input-cols (range 1 14)
            :output-cols [0]
            :decrease-output true) 
           {:unary-functions ['halfbias 'doublebias 'addbias 'subbias
                              'addval 'subval  'halfval 'doubleval
                              'inclr 'declr 'wait 'cut 'acttanh
                              'actsigmoid 'actsgn 'actsin 'actgauss]}))
(defn make-glass-classification
  []
  (make-ce (make-classification
            :experiment-name "Glass" 
            :dataset (io/read-dataset (str @dataset-prefix "glass.data"))
            :input-cols (range 1 10)
            :output-cols [10]
            :decrease-output true) 
           {:unary-functions  ['addbias 'subbias 'mult10bias 'mult-1bias
                               'mult0.1bias 'addval 'subval 'mult10val
                               'mult-1val 'mult0.1val 'inclr 'declr
                               'acttanh 'actsigmoid 'actsgn 'actsin
                               'actgauss],
            :max-genome-length 100}))



(defn make-yacht-regression 
  []
  (make-ce (make-regression
            :experiment-name "Yacht Hydrodynamics"
            :dataset (io/read-dataset (str @dataset-prefix 
                                           "yacht_hydrodynamics.data")
                                      :delim \space)
            :input-cols (range 0 6)
            :output-cols [6]
            :normalize-outputs true)
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


;;(def ccc (make-iris-classification))
;;(run ccc)
;; (show-log ccc)
;; (plot ccc)
