(ns nef.nes.neat.population
  (:require [nef.nes.neat 
             [evolution-parameters :as ep]
             [individual :as ind]
             [species :as spec]
             [genome :as gen]
             [operators :as op]]))

(set! *warn-on-reflection* true)

(defrecord Population
    [champions
     generation
     solved-at
     stats
     species
     current-dt])


(defn new-population
  []
  (->Population [] 0 nil [] [] @ep/dt))

(defn- get-individuals
  [population]
  (mapcat #(:members %)
          (:species population)))

(defn- evaluate 
  [individuals]
  (mapv (fn [ind]
          (let [{:keys [fitness solved? success-rate]} (@ep/fitness-fun (:genome ind))]
            (assoc ind 
              :raw-fitness fitness
              :fitness fitness
              :solved? solved?
              :success-rate success-rate)))
        individuals))

(defn- get-evaluated-individuals
  [population]
  (evaluate (loop [pop (get-individuals population)
                   i (count pop)]
              (if (< i @ep/population-size)
                (recur (conj pop (ind/new-individual :input @ep/input-count :output @ep/output-count)) (inc i))
                pop))))

(defn- put-in-species
  [population ind]
  (loop [i 0]
    (if (< i (count (:species @population)))
      (if (< (gen/delta (:genome ind) 
                        (:genome (:representative (nth (:species @population) i))))
             (:current-dt @population))
        (swap! population update-in [:species i :members] conj ind)
        (recur (inc i)))
      (swap! population #(assoc % :species (conj (:species %) (spec/new-species ind)))))))

(defn- speciate
  [population inds]
  (swap! population (fn [pop] 
                      (assoc pop :species
                             (mapv (fn [spec]
                                     (assoc spec
                                       :representative (rand-nth (:members spec))
                                       :members []
                                       :age (inc (:age spec)))) (:species pop)))))
  (dorun (doseq [ind inds]
           (put-in-species population ind)))
  (swap! population #(assoc % :species 
                            (vec (filter (fn [x] (> (count (:members x)) 0)) (:species %)))))
  (if (< 0 @ep/target-species)
    (swap! population #(assoc % :current-dt 
                              (+ (:current-dt %) (cond 
                                                  (< (count (:species @population)) @ep/target-species) 
                                                  (- @ep/dt-delta)
                                                  (> (count (:species @population)) @ep/target-species)
                                                  @ep/dt-delta
                                                  :else 0))))))

(defn- find-best
  [inds population]
  (let [champ (first (sort-by :fitness > inds))
        solutions (filter :solved? inds)]
    (swap! population #(assoc % 
                         :champions (conj (:champions %) champ)
                         :solved-at (or (:solved-at %)
                                        (when (> (count (filter :solved? inds)) 0)
                                          (:generation %)))))
    solutions))

(defn- mean 
  [coll]
  (if (< 0 (count coll))
    (/ (reduce + coll) (count coll))
    0.0))

(defn- tournament
  [pool]
  (first (sort-by :fitness > (take (min (count pool) 
                                        @ep/tournament-k)
                                   (shuffle pool)))))

(defn- generate-offspring 
  [pool]
    (if (< (rand) @ep/mutate-only-prob)
      (let [p1 (tournament pool)]
        (ind/new-individual :genome (op/mutation (:genome p1))
                            :id (:id  p1)))
      (ind/new-individual :genome  (let [p1 (tournament pool)
                                         p2 (tournament pool)
                                         ch (op/crossover (:genome p1)
                                                          (:genome p2)
                                                          (- (:fitness p1)
                                                             (:fitness p2)))]
                                     (if (< (rand) @ep/mate-only-prob)
                                       ch
                                       (op/mutation ch))))))

(defn- produce-offspring 
  [species]
  (let [sorted-inds (sort-by :fitness > (:members species))
        keep (max 1 (int (Math/round ^double (* (count (:members species))
                                                @ep/survival-rate-in-species))))
        pool (vec (take keep sorted-inds))]
    (loop [members (if (and @ep/elitism (> (count sorted-inds)
                                           @ep/min-elitism-size))
                     [(first sorted-inds)]
                     [])]
      (if (< (count members) (:offspring species))
        (recur (conj members (generate-offspring pool)))
        (assoc species :members members)))))

(defn- reproduce
  [pop]
  ;; Share the fitness
  (swap! pop (fn [population]
               (assoc population :species 
                      (mapv (fn [spec]
                              (let [memcnt (count (:members spec))]
                                (assoc spec :members 
                                       (mapv (fn [ind]
                                               (assoc ind 
                                                 :fitness (/ (:fitness ind)
                                                             memcnt))) 
                                             (:members spec)))))
                            (:species population)))))
  ;; Calculate basic stats
  (swap! pop
         (fn [population] 
           (assoc population
             :species (mapv (fn [spec]
                              (let [fits (sort > (mapv :fitness (:members spec)))]
                                (assoc spec 
                                  :max-fitness-prev (:max-fitness spec)
                                  :avg-fitness (mean fits)
                                  :max-fitness (first fits)
                                  :no-improvement-age (if (<= (first fits) 
                                                              (:max-fitness spec))
                                                        (inc (:no-improvement-age spec))
                                                        0)
                                  :has-best (> (count (filter #(= (:id %) 
                                                                  (:id (last (:champions population))))
                                                              (:members spec)))0))))
                            (:species population)))))
  ;; Remove old lesser species
  (swap! pop 
         (fn [population]
           (assoc population
             :species (vec (filter #(or (< (:no-improvement-age %) @ep/stagnation-age)
                                        (:has-best %))
                                   (:species population)))
             :avg-fitness (mean (mapv :avg-fitness (:species population))))))
  ;; Help the young and penalize the old ones
  (swap! pop update-in [:species]
         #(mapv (fn [species]
                  (assoc species :avg-fitness (* (:avg-fitness species) 
                                                 (cond 
                                                  (< (:age species) @ep/young-age) @ep/young-age-multiplier
                                                  (> (:age species) @ep/old-age) @ep/old-age-multiplier
                                                  :else 1)))) %))
  ;; Count expected number of offsprings
  (let [total-avg (reduce + (map :avg-fitness (:species @pop)))]
    (swap! pop update-in [:species]
           #(mapv (fn [species]
                    (assoc species :offspring (int (Math/round ^double (* @ep/population-size
                                                                  (/ (:avg-fitness species)
                                                                     total-avg)))))) %)))
  ;; Remove species with less than 1 offsprings
  (swap! pop #(assoc % :species (vec (filter (fn [s] (> (:offspring s) 0)) (:species %)))))
  ;; Clear the gene-pool (used to identify same connection genes)
  (reset! ep/gene-pool {})
  ;; Produce the offspring
  (swap! pop #(assoc % :species (mapv produce-offspring (:species %)))))

(defn evolve
  [population]
  (let [inds (get-evaluated-individuals @population)]
    (speciate population inds)
    (let [solutions (find-best inds population)]
      (reproduce population)
      (let [ms (first (sort-by :success-rate > (mapcat :members (:species @population))))]
        (swap! population assoc 
               :generation (inc (:generation @population))
               :stats (conj (:stats @population)
                            {:generation (:generation @population)
                             :champion (last (:champions @population))
                             :species (mapv (fn [spec]
                                              {:id (:id spec)
                                               :has-best (:has-best spec)
                                               :max-fitness (:max-fitness spec)
                                               :avg-fitness (:avg-fitness spec)
                                               :size (count (:members spec))}) (:species @population))
                             :solutions solutions
                             :success-rate (:success-rate ms)
                             :most-successful ms
                             :current-dt (:current-dt @population)})))))
  @population)




