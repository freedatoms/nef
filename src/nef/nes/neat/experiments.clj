(ns nef.nes.neat.experiments
  (:gen-class)
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

                                        ;(set! *warn-on-reflection* true)

(def dataset-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/Implementace/datasets/")





;; Problems
(defn xor-problem
  []
  (dosync
   (ref-set ep/input-count 2)
   (ref-set ep/output-count 1)
   (ref-set ep/weight-range [-26.0 26.0])
   (ref-set ep/fitness-fun 
            (fn [genome]
              {:fitness (Math/pow (max 0.000000001
                                       (- 4 
                                          (reduce + 
                                                  (mapv #(Math/abs ^float (- %2 
                                                                             (first (net/evaluate-neural-net-with-activation-cycles genome %1 10))))
                                                        [[0 0][1 0][0 1][1 1]]
                                                        [0 1 1 0])))) 2) 
               :solved? (and (== 0 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 0] 100))))
                             (== 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [1 0] 100))))
                             (== 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 1] 100))))
                             (== 0 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles 
                                                              genome [1 1] 100)))))
               :success-rate (/ (+ (- 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                                   genome [0 0] 100))))
                                   (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [1 0] 100)))
                                   (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 1] 100)))
                                   (- 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                                   genome [1 1] 100)))))
                                4)})))
  (evo/evolution :name "xor"))




(defn iris
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "iris.data")))]
    (gui/set-new-settings )
    (dosync
     (ref-set ep/fitness-fun (make-classification-fitness 
                              (normalize-columns (co/to-vect (co/sel dataset :cols [0 1 2 3])))
                              (mapv #(int %) (co/to-vect  (co/sel dataset :cols [4]))))))
    (evo/evolution :name "iris")))

(defn wine 
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "wine.data")))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 3, :weight-range [-5.0 5.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 2.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.3, :old-age 30, :clamp-weight-factor 0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.1, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 13, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (dosync 
     (ref-set ep/fitness-fun
              (make-classification-fitness (normalize-columns (co/to-vect (co/sel dataset :cols (range 1 14))))
                                           (mapv #(int (dec %)) (co/to-vect  (co/sel dataset :cols 0))))))
    (evo/evolution :name "wine")))

(defn glass 
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "glass.data")))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 7, :weight-range [-1.0 1.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 0.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.3, :old-age 30, :clamp-weight-factor 1.0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.2, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.3, :input-count 9, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1})
    (dosync 
     (ref-set ep/add-connection-prob 0.05)
     (ref-set ep/fitness-fun
              (make-classification-fitness (normalize-columns (co/to-vect (co/sel dataset :cols (range 1 10))))
                                           (mapv #(int (dec %)) (co/to-vect  (co/sel dataset :cols 10))))))
    (evo/evolution :name "glass")))


(defn regression-yacht []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "yacht_hydrodynamics.data") :delim \space))
        input (normalize-columns (co/to-vect (co/sel dataset :cols (range 0 6))))
        output (mapv vector (normalize-column* (co/to-vect (co/sel dataset :cols 6))))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.05, :c1 1.0, :output-count 1, :weight-range [-1.0 1.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 0.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 1.0, :old-age 30, :clamp-weight-factor 0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.05, :tournament-k 4, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 6, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (dosync
     (ref-set ep/fitness-fun (make-regression-fitness input output 0.001)))
    (evo/evolution :name "yacht")))


(defn maze []
  (let [fr (JFrame.)]
    (.setSize fr 600 600)
    (dosync 
     (ref-set ep/fitness-fun maze-fitness))
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 3, :weight-range [-5.0 5.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 2.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.2, :old-age 30, :clamp-weight-factor 1.0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.4, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 5, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (evo/evolution :name "maze"
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
                               true)
                           ]
                       (.setTitle fr (str "Maze fitness: " (:fitness ind)))
                       (.removeAll (.getContentPane fr))
                       (.add (.getContentPane fr) mv)
                       (when (not (.isShowing fr))
                         (.setVisible fr true))
                       (.saveImage mv (str filename "-maze.png") 800 800)
                       (.revalidate fr))))))

(maze)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (dotimes [_ 1]
    (prn :iris----------------------------------------------)
    ;;  (iris)
    (prn :wine----------------------------------------------)
    ;;  (wine)
    ;; (prn :yacht---------------------------------------------)
    ;; (regression-yacht)
    ;; (prn :maze----------------------------------------------)
    ;;(maze)
    #_(prn :glass---------------------------------------------)
    #_(glass)))
