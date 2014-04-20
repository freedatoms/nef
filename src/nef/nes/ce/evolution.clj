(ns nef.nes.ce.evolution
  (:require [nef.nes.ce
             [cellular-encoding-j :as ce]
             [fitness :as f]
             [population :as p]
             [individual :as i]]
            [rhizome [viz :as viz]]
            [incanter
             [charts :as ch]
             [core :as co]])
  (:use nef.graphviz-enabled))


(defn- IO-neurons-count [ind]
  (let [net (ce/ce->neural-net (:genome ind))]
    [(count (:input net))
     (count (:output net))]))

(defn fitness-sharing [pop]
  (let [m (atom {})]
    (doseq [i pop]
      (let [k (IO-neurons-count i)]
        (swap! m #(assoc % k (inc (% k 0))))))
    (mapv (fn [i]
            (i/new-individual :genome (:genome i)
                              :fitness (/ (:fitness i)
                                          (@m (IO-neurons-count i))))) pop)))

(defn simple-evolution [N-inds N-generations & {:keys [best-individual-function log]}]
  (let [cur-pop (atom (p/make-population N-inds))
        new-pop (atom [])
        best (atom (transient []))
        worst (atom (transient []))
        genome-frame (viz/create-frame "Best Genome")
        net-frame (viz/create-frame "Best Net")]
    (dotimes [g N-generations]
      (time (do
              (reset! new-pop (vec (doall (flatten
                                           (mapv #(into [%]
                                                        (mapv i/mutate
                                                              (i/crossover % (p/tournament-selection @cur-pop 5))))
                                                 @cur-pop)))))
              #_(swap! new-pop fitness-sharing)
              (let [np @new-pop]
                (reset! cur-pop (vec (sort-by :fitness > (repeatedly N-inds #(p/tournament-selection np 5))))))
              (reset! new-pop [])
              (swap! best #(conj! % (:fitness (first @cur-pop))))
              (swap! worst #(conj! % (:fitness (last @cur-pop))))
              (send-off log conj {:generation g,
                                  :performance (:fitness (first @cur-pop)),
                                  :max-performance (:fitness (first @cur-pop))
                                  :mean-performance(float (reduce + (mapv #(/ (:fitness %) N-inds) @cur-pop))),
                                  :min-performance (:fitness (last @cur-pop)),
                                  :best-genome-length (count (flatten (:genome (first @cur-pop)))),
                                  :mean-genome-length (float (reduce + (mapv #(/ (count (flatten (:genome %))) N-inds) @cur-pop)))})
              (with-precision 5
                (printf "Gen: %5d   Best: %+5f Avg: %+5f   Worst: %+5f   Best G. Length: %+5d  Avg Genome Length: %+5f    "
                        g
                        (:fitness (first @cur-pop))
                        (float (reduce + (mapv #(/ (:fitness %) N-inds) @cur-pop)))
                        (:fitness (last @cur-pop))
                        (count (flatten (:genome (first @cur-pop))))
                        (float (reduce + (mapv #(/ (count (flatten (:genome %))) N-inds) @cur-pop)))))))

      (when best-individual-function
        (best-individual-function (first @cur-pop) g))
      (view (first @cur-pop) genome-frame
            (str "Genome in generation: " g " with fitness: " (:fitness (first @cur-pop))))
      (view (ce/ce->neural-net (:genome (first @cur-pop))) net-frame
            (str "Neural Net in generation: " g " with fitness: " (:fitness (first @cur-pop)))))))


