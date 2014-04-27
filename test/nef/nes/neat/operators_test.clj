(ns nef.nes.neat.operators-test
  (:use [nef.nes.neat
         genome
         evolution-parameters
         [gene :as gene]
  	 operators]
        [clojure
         test]))


(defmacro time-of
  [expr]
  `(do (printf "'%s': "  (first '~expr))
       (time ~expr)))

(let [genome1 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :bias] [2 :input] [3 :input] [4 :output] [5 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 1.0 true  1]
                               [2 4 1.0 false 2]
                               [3 4 1.0 true  3]
                               [2 5 1.0 true  4]
                               [5 4 1.0 true  5]
                               [1 5 1.0 true  8]]))
      genome2 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :bias] [2 :input] [3 :input] [4 :output] [5 :hidden]
                               [6 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 0.3 true  1]
                               [2 4 0.4 false 2]
                               [3 4 0.5 true  3]
                               [2 5 0.6 true  4]
                               [5 4 0.7 false 5]
                               [5 6 0.8 true  6]
                               [6 4 0.9 true  7]
                               [3 5 0.1 true  9]
                               [1 6 0.2 true 10]]))]
  (deftest add-node-test
    (reset! innovation-number 10)
    (reset! gene-pool {})
    (is (last (take 11 (iterate #'nef.nes.neat.operators/add-node genome2)))))
  (deftest add-connection-test
    (reset! innovation-number 10)
    (reset! gene-pool {})
    (is (last (take 11 (iterate #'nef.nes.neat.operators/add-connection genome2)))))
  (deftest crossover-test
    (is (crossover genome1 genome2 0)))
  (deftest mutation-test
    (is (mutation genome1)))
  (deftest mutate-and-crossover-test
    (dotimes [_ 100]
      (is  (crossover
            (last (take 11 (iterate mutation genome1)))
            (last (take 11 (iterate mutation genome2)))
            0)))
    ))



