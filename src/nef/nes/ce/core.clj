(comment (ns ce.core
           
           (:require 
                     
                     [rhizome
                      [viz :as viz]]
                     [taoensso.timbre.profiling :as profiling :refer (p profile)]
                     [taoensso.timbre :as timbre]
                     [incanter
                      [core :as co]
                      [io :as io]                
                      [datasets :as dat]])
           (:use cega.graphviz-enabled)
           (:import [eu.tm3da.thesis
                     DiscreteMaze
                     DiscreteMazeViewer]
                    javax.swing.JFrame))

         
         (def file-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/results/")

         (defn xor-problem []
           (prn "xor-problem")
           (let [pop-size 10000
                 generations 100]
             (binding [ce/*lifespan* 1
                       t/*unary-function* ['val- 'incbias]
                       ;; i/*max-genome-length* 11
                       ;; i/*mutation-rate* 0.01
                       ;; f/*inputs* [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]]
                       ]
               
               (time (binding [f/*outputs* (mapv #(vector (rem (reduce + %) 2)) f/*inputs*)]
                       (profile :info "Evolution" (simple-evolution pop-size generations))
                       #_(co/save (profile :info "Evolution" (simple-evolution pop-size generations))
                                  (str "/home/frydatom/tmp/" (java.util.Date.) " -plot-" pop-size "-" generations ".png")))))))



         (defn classification-iris []
           (prn "classification-iris")
           (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "iris.data")))
                 filename (str file-prefix "cega-" "iris" "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
             (binding [f/*inputs* (co/to-vect  (co/sel dataset :cols [0 1 2 3]))
                       f/*outputs* (mapv #(int %) (co/to-vect  (co/sel dataset :cols [4])))
                       ce/*default-activation-function* ce/act-tanh
                       ce/*lifespan* 1
                       *graphviz-dpi* 50
                       i/*max-genome-length* 50
                       f/*fitness-function* f/classification-fitness
                       t/*binary-function* ['seq 'par; 'blife
                                            ]
                       t/*unary-function* ['halfbias 'doublebias 'addbias 'subbias
                                        ;'mult10bias 'mult-1bias 'mult0.1bias ; 'mult1.25bias 'mult0.8bias
                                           'addval 'subval  'halfval 'doubleval
                                        ;'mult10val 'mult-1val 'mult0.1val ;'mult1.25val 'mult0.8val
                                           'inclr 'declr
                                        ;'wait ;'cut 
                                           'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss
                                           ]]
               (simple-evolution 450 100;1000 100
                                 :best-individual-function
                                 (fn [ind gen]
                                   (let [net (ce/evaluate-tree-grammar (:genome ind))
                                         fn (format filename gen (:fitness ind))]
                                     (save-dot (:genome ind) (str fn "-grammar.dot"))
                                     (save-image (:genome ind) (str fn "-grammar.png"))
                                     (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                     (save-image (ce/graph->neural-net net) (str fn "-net.png"))))))))

         (defn classification-wine []
           (prn "classification-wine")
           (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "wine.data")))
                 filename (str file-prefix "cega-" "wine" "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
             (binding [f/*inputs* (co/to-vect  (co/sel dataset :cols (range 1 14)))
                       f/*outputs* (mapv #(int %) (co/to-vect  (co/sel dataset :cols 0)))
                       ce/*default-activation-function* ce/act-tanh
                       ce/*lifespan* 1
                       *graphviz-dpi* 50
                       i/*max-genome-length* 50
                       f/*fitness-function* f/classification-fitness
                       t/*binary-function* ['seq 'par ; 'blife
                                            ]
                       t/*unary-function* ['halfbias 'doublebias 'addbias 'subbias
                                           'addval 'subval  'halfval 'doubleval
                                           'inclr 'declr 'wait 'cut 
                                           'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss
                                           ]]
               (simple-evolution 450 100;1000 100
                                 :best-individual-function
                                 (fn [ind gen]
                                   (let [net (ce/evaluate-tree-grammar (:genome ind))
                                         fn (format filename gen (:fitness ind))]
                                     (save-dot (:genome ind) (str fn "-grammar.dot"))
                                     (save-image (:genome ind) (str fn "-grammar.png"))
                                     (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                     (save-image (ce/graph->neural-net net) (str fn "-net.png"))))))))

         (defn classification-glass []
           (prn "classification-glass")
           (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "glass.data")))
                 input (into-array (map double-array (co/to-vect (co/sel dataset :cols (range 1 10)))))
                 output (int-array (map dec (co/to-vect (co/sel dataset :cols 10))))
                 filename (str file-prefix "cega-" "glass" "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
             (binding [f/*inputs* input
                       f/*outputs* output
                       ce/*default-activation-function* ce/act-tanh
                       ce/*lifespan* 1
                       *graphviz-dpi* 50
                       i/*max-genome-length* 100
                       f/*fitness-function* f/classification-fitness
                       t/*binary-function* ['seq 'par ; 'blife
                                            ]
                       t/*unary-function* ['addbias 'subbias 'mult10bias 'mult-1bias
                                           'mult0.1bias 'addval 'subval 'mult10val
                                           'mult-1val 'mult0.1val 'inclr 'declr
                                           'acttanh 'actsigmoid 'actsgn 'actsin
                                           'actgauss]]
               (simple-evolution 450 100;1000 100
                                 :best-individual-function
                                 (fn [ind gen]
                                   (let [net (ce/evaluate-tree-grammar (:genome ind))
                                         fn (format filename gen (:fitness ind))]
                                     (save-dot (:genome ind) (str fn "-grammar.dot"))
                                     (save-image (:genome ind) (str fn "-grammar.png"))
                                     (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                     (save-image (ce/graph->neural-net net) (str fn "-net.png"))))))
             ))

         (defn normalize-column [col]
           (let [min (apply min col)
                 max (apply max col)
                 diff (- max min)]
             (mapv (fn [x] (/ (- x min) diff)) col)))

         (defn regression-yacht []
           (prn "regression-yacht")
           (let [dataset (co/to-matrix )
                 input (into-array (map double-array (co/to-vect (co/sel dataset :cols (range 0 6)))))
                 output (normalize-column (co/to-vect (co/sel dataset :cols 6)))
                 filename (str file-prefix "cega-" "yacht" "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
             (binding [f/*inputs* input
                       f/*outputs* output
                       ce/*default-activation-function* ce/act-tanh
                       ce/*lifespan* 1
                       *graphviz-dpi* 50
                       i/*max-genome-length* 50
                       f/*fitness-function* f/regression-fitness
                       t/*binary-function* ['seq 'par ; 'blife
                                            ]
                       t/*unary-function* ['addbias 'subbias 'mult10bias 'mult-1bias
                                           'mult0.1bias 'addval 'subval 'mult10val 
                                           'mult-1val 'mult0.1val 'inclr 'declr
                                           'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss]]
               (simple-evolution 450 100;1000 100   
                                 :best-individual-function
                                 (fn [ind gen]
                                   (binding [f/*fitness-function* f/regression-fitness2]
                                     (let [nind (i/new-individual :genome (:genome ind))
                                           net (ce/evaluate-tree-grammar (:genome nind))
                                           fn (format filename gen (:fitness nind))]
                                       (save-dot (:genome nind) (str fn "-grammar.dot"))
                                       (save-image (:genome nind) (str fn "-grammar.png"))
                                       (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                       (save-image (ce/graph->neural-net net) (str fn "-net.png")))))))))



         (defn maze []
           (prn "maze")
           (binding [ce/*default-activation-function* ce/act-tanh
                     ce/*lifespan* 1
                     *graphviz-dpi* 50
                     i/*max-genome-length* 30
                     f/*fitness-function* f/maze-fitness
                     t/*binary-function* ['seq 'par ; 'blife
                                          ]
                     t/*unary-function* ['addbias 'subbias 'mult10bias 'mult-1bias 
                                         'mult0.1bias 'addval 'subval 'mult10val 
                                         'mult-1val 'mult0.1val 'inclr 'declr 
                                         'acttanh 'actsigmoid 'actsgn 'actsin 'actgauss]]
             (let [fr (JFrame.)
                   filename (str file-prefix "cega-" "maze" "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
               (.setSize fr 600 600)
               (simple-evolution 450 100
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
                                     (save-dot (:genome ind) (str fn "-grammar.dot"))
                                     (save-image (:genome ind) (str fn "-grammar.png"))
                                     (save-dot (ce/graph->neural-net net) (str fn "-net.dot"))
                                     (save-image (ce/graph->neural-net net) (str fn "-net.png"))
                                     (.saveImage mv (str fn "-maze.png") 800 800)
                                     (.revalidate fr)
                                     )))))
           
           )


         (defn -main
           [& args]
           (alter-var-root #'*read-eval* (constantly false))
           #_(timbre/set-level! :warn)
           #_(xor-problem)
           (dotimes [_ 5]
             #_(classification-iris)
             #_(classification-wine)
             #_(classification-glass)
             (regression-yacht)
             #_(maze)))




         )
