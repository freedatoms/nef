(ns nef.nes.neat.genome-test
  (:use [nef.nes.neat
         genome
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
                              [[1 4 1.0 true  1]
                               [2 4 1.0 false 2]
                               [3 4 0.0 true  3]
                               [2 5 1.0 true  4]
                               [5 4 1.0 false 5]
                               [5 6 1.0 true  6]
                               [6 4 1.0 true  7]
                               [3 5 1.0 true  9]
                               [1 6 1.0 true 10]]))]
  (deftest excess
    (is  (= [9 10] (mapv :innov (time-of (#'nef.nes.neat.genome/excess genome1 genome2))))))
  (deftest excess-count
    (is  (= 2 (time-of (#'nef.nes.neat.genome/excess-count genome1 genome2)))))
  (deftest disjoint
    (is (= [6 7 8] (mapv :innov (time-of (#'nef.nes.neat.genome/disjoint genome1 genome2))))))
  (deftest disjoint-count
    (is (= 3 (time-of (#'nef.nes.neat.genome/disjoint-count genome1 genome2)))))
  (deftest weight-diff
    (is (= 0.2 (time-of (#'nef.nes.neat.genome/weight-diff genome1 genome2)))))
  (deftest test-matching-genes
    (is (= [[1 1] [2 2] [3 3] [4 4] [5 5] [nil 6] [nil 7] [8 nil] [nil 9] [nil 10]]
           (mapv (fn [[i1 i2]] [(:innov i1) (:innov i2)])
                 (time-of (nef.nes.neat.genome/match-genes genome1 genome2)))))))









