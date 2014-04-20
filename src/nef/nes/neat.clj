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
        nef.graphviz-enabled)
  (:import [nef.nes.task.maze
            DiscreteMaze
            DiscreteMazeViewer]
           javax.swing.JFrame))


(defn normalize-column*
  "Performs min-max normalization to interval [0,1]"
  [col]
  (let [min (apply min col)
        max (apply max col)
        diff (- max min)]
    (mapv (fn [x] (double (/ (- x min) diff))) col)))

(defn normalize-column 
  "Performs min-max normalization to interval [-1,1]"
  [col]
  (let [min (apply min col)
        max (apply max col)
        diff (- max min)]
    (mapv (fn [x] (double (+ -1 (* 2 (/ (- x min) diff))))) col)))

(defn normalize-columns
  [cols]
  (let [cls (loop 
                [[x & xs] (rest cols)
                 cs (mapv vector (first cols))]
              (if xs
                (recur xs (mapv conj cs x))
                (mapv conj cs x)))]
    (apply mapv (fn [& xs] (vec xs))
           (mapv normalize-column cls))))

(defn- run-classification
  [neat run-params]
  (let [dataset (co/to-matrix (:data (:problem-domain neat)))
        outputs (mapv #(int %) (co/to-vect
                                (co/sel dataset :cols
                                        (:output-cols (:problem-domain neat)))))]
    (gui/set-new-settings (:options neat))
    (dosync
     (ref-set ep/fitness-fun (f/make-classification-fitness 
                              (normalize-columns 
                               (co/to-vect (co/sel dataset :cols 
                                                   (:input-cols (:problem-domain neat)))))
                              (if (:decrease-outputs (:problem-domain neat))
                                (mapv dec outputs)
                                outputs))))
    (evo/evolution :name (:name (:problem-domain neat))
                   :save-images (:save-images run-params)
                   :save-individuals (:save-individuals run-params)
                   :file-prefix (:file-prefix run-params)
                   :log (:log neat))))

(defn- run-regression
  [neat run-params]
  (let [dataset (co/to-matrix (:data (:problem-domain neat)))]
    (gui/set-new-settings (:options neat))
    (dosync
     (ref-set ep/fitness-fun (f/make-regression-fitness 
                              (normalize-columns 
                               (co/to-vect (co/sel dataset :cols 
                                                   (:input-cols (:problem-domain neat)))))
                              (mapv vector (normalize-column*
                                            (co/to-vect
                                             (co/sel dataset :cols
                                                     (:output-cols (:problem-domain neat)))))))))
    (evo/evolution :name (:name (:problem-domain neat))
                   :save-images (:save-images run-params)
                   :save-individuals (:save-individuals run-params)
                   :file-prefix (:file-prefix run-params)
                   :log (:log neat))))


(defn- run-custom-fitness
  [neat run-params]
  (throw "This is not ready yet"))

(defn- run-maze
  [neat run-params]
  (let [fr (JFrame.)]
    (.setSize fr 600 600)
    (dosync 
     (ref-set ep/fitness-fun (if (:normalize (:problem-domain neat))
                               f/maze-fitness-using-normalized-inputs
                               f/maze-fitness)))
    (gui/set-new-settings (:options neat))
    (evo/evolution :name "maze"
                   :log (:log neat)
                   :save-images (:save-images run-params)
                   :save-individuals (:save-individuals run-params)
                   :file-prefix (:file-prefix run-params)
                   :do-with-best
                   (fn [ind filename]
                     (let [m (DiscreteMaze.)
                           mv (DiscreteMazeViewer. 
                               m 
                               (fn [in]
                                 (double-array
                                  (net/evaluate-neural-net-with-activation-cycles 
                                   (:genome ind)
                                   in
                                   100)))
                               (boolean (:normalize (:problem-domain neat))))
                           ]
                       (.setTitle fr (str "Maze fitness: " (:success-rate ind)))
                       (.removeAll (.getContentPane fr))
                       (.add (.getContentPane fr) mv)
                       (when (not (.isShowing fr))
                         (.setVisible fr true))
                       (.saveImage mv (str filename "-maze.png") 800 800)
                       (.revalidate fr))))))


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
    (case (:type (:problem-domain this))
      :classification (run-classification this run-params)
      :regression (run-regression this run-params)
      :maze (run-maze this run-params)
      :custom-fitness (run-custom-fitness this run-params))
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
                              :title (str (humanize-keyword (:type problem-domain))
                                          " of " 
                                          (:name problem-domain)
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
  (make-neat (make-classification-of "Iris"
                                   (io/read-dataset (str dataset-prefix "iris.data"))
                                   [0 1 2 3] 
                                   [4]) 
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
  (make-neat (make-classification-of "Wine"
                                   (io/read-dataset (str dataset-prefix "wine.data")) 
                                   (range 1 14)
                                   [0]
                                   :decrease-outputs true) 
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
  (make-neat (make-classification-of "Glass" 
                                   (io/read-dataset (str dataset-prefix "glass.data"))
                                   (range 1 10)
                                   [10]
                                   :decrease-outputs true) 

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
  (make-neat (make-regression-of "Yacht Hydrodynamics"
                               (io/read-dataset (str dataset-prefix 
                                                     "yacht_hydrodynamics.data")
                                                :delim \space)
                               (range 0 6)
                               [6])     
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
            :normalize true)
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
;; (run ccc)
;; (show-log ccc)
;; (plot ccc)

