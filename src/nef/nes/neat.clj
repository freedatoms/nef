(ns nef.nes.neat
  (:require [incanter 
             [core :as co]
             [charts :as ch]
             [datasets :as d]
             [io :as io]]
            [nef.nes.neat
             [fitness :as f]
             [evolution :as evo]
             [evolution-parameters :as ep]
             [gui :as gui]
             [neural-net :as net]])
  (:use nef.neprotocol
        nef.problem-domain
        nef.utils
        nef.graphviz-enabled)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer]
           javax.swing.JFrame))


(defn- run-custom-fitness
  [neat run-params]
  (gui/set-new-settings (:options neat))
  (dosync
   (ref-set ep/fitness-fun
            (fn [individual]
              (let [success-rate  ((get-fitness-function
                                    (:problem-domain neat)
                                    (fn [ind in]
                                      (net/evaluate-neural-net-with-activation-cycles ind in 100)))
                                   individual)]
                {:fitness success-rate, ;this is fitness, it gets adjusted due to niching
                 :success-rate success-rate ;this is "unadjustable" fitness
                 :solved (and (:solved-theshold run-params) (> success-rate (:solved-threshold)))}))))
  (evo/evolution :name (get-experiment-name (:problem-domain neat))
                 :save-images (:save-images run-params)
                 :save-individuals (:save-individuals run-params)
                 :file-prefix (:file-prefix run-params)
                 :log (:log neat)
                 :do-with-best (get-do-with-best-individual-function
                                (:problem-domain neat)
                                (fn [ind in]
                                  (net/evaluate-neural-net-with-activation-cycles
                                   (:genome ind) in 100)))))


(defrecord NEAT
    [problem-domain
     options
     log]
  NeuroEvolution
  (run [this] 
    (run this {:save-images false, 
               :save-individuals false,
               :file-prefix "/tmp/"}))
  (run [this run-params]
    (send (:log this) (fn [_] []))
    (run-custom-fitness this run-params)
    (send (:log this) co/to-dataset)
    (await (:log this)))

  (save-log-to-csv [this filename]
    (co/save (get-log this) filename))

  (show-log [this]
    (co/view @(:log this)))

  (plot [this]
    (plot this
          ["Generations" "Performance"] 
          [[:generation :performance]]))
  (plot [this [x-label y-label] xy-pairs]
    (let [[[xo yo] & xyp] xy-pairs]
      (loop [[[x y] & rest] xyp
             plot (ch/xy-plot (co/$ xo (get-log this))
                              (co/$ yo (get-log this))
                              :title (str (get-name problem-domain)
                                          " of " 
                                          (get-experiment-name problem-domain)
                                          " using NEAT")
                              :legend true
                              :x-label x-label
                              :y-label y-label
                              :series-label (humanize-keyword yo))]
        (if (and (not rest) (not x))
          (co/view plot)
          (recur rest (ch/add-lines plot 
                                    (co/$ x (get-log this))
                                    (co/$ y (get-log this))
                                    :series-label (humanize-keyword y)))))))

  (get-log [this]
    @(:log this)))

(defn make-neat [problem-domain options]
  (NEAT. problem-domain options (agent [])))

(def dataset-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/Implementace/datasets/")

(defn make-iris-classification
  []
  (make-neat (make-classification
              :experiment-name"Iris"
              :dataset (io/read-dataset (str dataset-prefix "iris.data"))
              :input-cols [0 1 2 3] 
              :output-col [4]
              :normalize-inputs true) 
             {:add-connection-prob 0.05,
              :add-node-prob 0.03,
              :c1 1.0,
              :c2 1.0,
              :c3 0.3,
              :clamp-weight-factor 1.0,
              :connection-density 0.0,
              :crossover-prob 0.75,
              :disable-in-crossover 0.75,
              :dt 3.0,
              :dt-delta 0.4,
              :elitism true,
              :generation-count 100,
              :input-count 4,
              :interspecies-mating-prob 0.001,
              :mate-by-choosing-prob 0.6,
              :mate-only-prob 0.2,
              :min-elitism-size 5,
              :mutate-only-prob 0.25,
              :mutate-weights-perturb-prob 0.9,
              :mutate-weights-perturb-sigma 2.5,
              :mutate-weights-prob 0.8,
              :mutation-prob 0.25,
              :old-age 30,
              :old-age-multiplier 0.2,
              :output-count 3,
              :population-size 450,
              :stagnation-age 15,
              :survival-rate-in-species 0.2,
              :target-species 0,
              :tournament-k 1,
              :visualize-genome-with [],
              :weight-range [-1.0 1.0],
              :young-age 10,
              :young-age-multiplier 1.2}))



(defn make-wine-classification
  []
  (make-neat (make-classification
              :experiment-name"Wine"
              :dataset (io/read-dataset (str dataset-prefix "wine.data")) 
              :input-cols (range 1 14)
              :output-col [0]
              :normalize-inputs true
              :decrease-output true) 
             {:add-connection-prob 0.05,
              :add-node-prob 0.03,
              :c1 1.0,
              :c2 1.0,
              :c3 0.3,
              :clamp-weight-factor 0,
              :connection-density 0.0,
              :crossover-prob 0.75,
              :disable-in-crossover 0.75,
              :dt 3.0,
              :dt-delta 0.1,
              :elitism true,
              :generation-count 100,
              :input-count 13,
              :interspecies-mating-prob 0.001,
              :mate-by-choosing-prob 0.6,
              :mate-only-prob 0.2,
              :min-elitism-size 5,
              :mutate-only-prob 0.25,
              :mutate-weights-perturb-prob 0.9,
              :mutate-weights-perturb-sigma 2.5,
              :mutate-weights-prob 0.8,
              :mutation-prob 0.25,
              :old-age 30,
              :old-age-multiplier 0.2,
              :output-count 3,
              :population-size 450,
              :stagnation-age 15,
              :survival-rate-in-species 0.2,
              :target-species 0,
              :tournament-k 1,
              :visualize-genome-with [],
              :weight-range [-5.0 5.0],
              :young-age 10,
              :young-age-multiplier 1.2}))


(defn make-glass-classification
  []
  (make-neat (make-classification
              :experiment-name"Glass" 
              :dataset (io/read-dataset (str dataset-prefix "glass.data"))
              :input-cols (range 1 10)
              :output-col [10]
              :normalize-inputs true
              :decrease-output true) 
             {:add-connection-prob 0.05,
              :add-node-prob 0.03,
              :c1 1.0,
              :c2 1.0,
              :c3 0.3,
              :clamp-weight-factor 1.0,
              :connection-density 0.0,
              :crossover-prob 0.75,
              :disable-in-crossover 0.75,
              :dt 3.0,
              :dt-delta 0.2,
              :elitism true,
              :generation-count 100,
              :input-count 9,
              :interspecies-mating-prob 0.001,
              :mate-by-choosing-prob 0.6,
              :mate-only-prob 0.2,
              :min-elitism-size 5,
              :mutate-only-prob 0.25,
              :mutate-weights-perturb-prob 0.9,
              :mutate-weights-perturb-sigma 0.5,
              :mutate-weights-prob 0.8,
              :mutation-prob 0.25,
              :old-age 30,
              :old-age-multiplier 0.2,
              :output-count 7,
              :population-size 450,
              :stagnation-age 15,
              :survival-rate-in-species 0.2,
              :target-species 0,
              :tournament-k 1,
              :visualize-genome-with [],
              :weight-range [-1.0 1.0],
              :young-age 10,
              :young-age-multiplier 1}))



(defn make-yacht-regression 
  []
  (make-neat (make-regression
              :experiment-name"Yacht Hydrodynamics"
              :dataset (io/read-dataset (str dataset-prefix 
                                             "yacht_hydrodynamics.data")
                                        :delim \space)
              :input-cols (range 0 6)
              :output-cols [6]
              :normalize-inputs true
              :normalize-outputs true)     
             {:add-connection-prob 0.05,
              :add-node-prob 0.05,
              :c1 1.0,
              :c2 1.0,
              :c3 1.0,
              :clamp-weight-factor 0,
              :connection-density 0.0,
              :crossover-prob 0.75,
              :disable-in-crossover 0.75,
              :dt 3.0,
              :dt-delta 0.05,
              :elitism true,
              :generation-count 100,
              :input-count 6,
              :interspecies-mating-prob 0.001,
              :mate-by-choosing-prob 0.6,
              :mate-only-prob 0.2,
              :min-elitism-size 5,
              :mutate-only-prob 0.25,
              :mutate-weights-perturb-prob 0.9,
              :mutate-weights-perturb-sigma 0.5,
              :mutate-weights-prob 0.8,
              :mutation-prob 0.25,
              :old-age 30,
              :old-age-multiplier 0.2,
              :output-count 1,
              :population-size 450,
              :stagnation-age 15,
              :survival-rate-in-species 0.2,
              :target-species 0,
              :tournament-k 4,
              :visualize-genome-with [],
              :weight-range [-1.0 1.0],
              :young-age 10,
              :young-age-multiplier 1.2}))

(defn make-maze-reinforcement-learning
  []
  (make-neat (make-maze
              :normalize-inputs true)
             {:add-connection-prob 0.05,
              :add-node-prob 0.03,
              :c1 1.0,
              :c2 1.0,
              :c3 0.2,
              :clamp-weight-factor 1.0,
              :connection-density 0.0,
              :crossover-prob 0.75,
              :disable-in-crossover 0.75,
              :dt 3.0,
              :dt-delta 0.4,
              :elitism true,
              :generation-count 100,
              :input-count 5,
              :interspecies-mating-prob 0.001,
              :mate-by-choosing-prob 0.6,
              :mate-only-prob 0.2,
              :min-elitism-size 5,
              :mutate-only-prob 0.25,
              :mutate-weights-perturb-prob 0.9,
              :mutate-weights-perturb-sigma 2.5,
              :mutate-weights-prob 0.8,
              :mutation-prob 0.25,
              :old-age 30,
              :old-age-multiplier 0.2,
              :output-count 3,
              :population-size 450,
              :stagnation-age 15,
              :survival-rate-in-species 0.2,
              :target-species 0,
              :tournament-k 1,
              :visualize-genome-with [],
              :weight-range [-5.0 5.0],
              :young-age 10,
              :young-age-multiplier 1.2}))


;; (def ccc (make-maze-reinforcement-learning))
;;  (run ccc)
;; (show-log ccc)
;;  (plot ccc)

