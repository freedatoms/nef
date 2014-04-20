(ns nef.nes.neat.genome
  (:require [nef.nes.neat
             [gene :as gene]
             [evolution-parameters :as ep]]
            [rhizome
             [viz :as viz]
             [dot :as dot]])
  (:use [nef graphviz-enabled]
        [clojure.string :only (join)])
  (:import [javax.swing
            JFrame
            ImageIcon]))

(declare -do-graphviz-fun)

(defrecord Genome
    [node-genes
     connection-genes]
  GraphvizEnabled
  (save-image [this filename]
    (viz/save-image (-do-graphviz-fun
                     viz/graph->image
                     this)
                    filename))
  (save-dot [this filename]
    (spit filename
          (-do-graphviz-fun dot/graph->dot this)))
  (view [this]
    (view this (viz/create-frame "Neural Net")))
  (view [this frame]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setImage image-icon (-do-graphviz-fun viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true))))
  (view [this frame title]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setTitle frame title)
      (.setImage image-icon (-do-graphviz-fun viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true)))))

(defn- in-out
  [genes]
  (loop [last (first genes)
         rst  (next genes)
         res []]
    (if rst
      (recur (first rst) (next rst) (conj res [(:id last) [(:id (first rst))]]))
      res)))

(defn- genome->graph
  [^Genome gen]
  (loop [cg (:connection-genes gen)
         res {:node-gene [:conn-gene]}]
    (if cg
      (if (:enabled? (first cg))
        (recur (next cg) (update-in res [(:in (first cg))] conj (:out (first cg))))
        (recur (next cg) res))
      res)))

(defn- node-genes->records
  [genes]
  (mapv #(vector (:id %) (case (:type %)
                                     :input "input"
                                     :bias "bias"
                                     :hidden "hidden"
                                     :output "output")) genes))

(defn- conn-genes->records
  [genes]
  (mapv #(vector (:innov %)
                 (format "%s -&gt; %s" (:in %) (:out %))
                 (if (:enabled? %)
                   "enabled"
                   "disabled")
                 (format "%.3f" (:weight %))) genes))

(defn- weight-to-prop
  [weight [min max]]
  (let [neg-step (/ 4 (- min))
        pos-step (/ 4 max)
        neg-color-step (/ 100 (- min))
        pos-color-step (/ 100 max)]
    
    (if (< weight 0)
      {:penwidth (str (float (+ 1 (* (- weight) neg-step))))
       :color (format "#cc%02x%02x"
                      (int (- 101  (* (- weight) neg-color-step)))
                      (int (- 101 (* (- weight) neg-color-step))))}
      {:penwidth (str (float (+ 1 (* weight pos-step))))
       :color (format "#%02x%02xcc" 
                      #_(int (- 101 (* weight pos-color-step)))
                      (int (- 101 (* weight pos-color-step)))
                      (int (- 101 (* weight pos-color-step))))})))


(defn- -do-graphviz-fun
  [fun g]
  (let [input-cnt (count (mapv :id (filter #(#{:bias :input} (:type %)) (:node-genes g))))
        output-cnt (count (mapv :id (filter #(#{:output} (:type %)) (:node-genes g))))
        weights (into {} (mapv #(vector [(:in %) (:out %)] (:weight %)) (:connection-genes g)))
        wr (apply (juxt min max) (vals weights))]
    (fun (into (mapv :id (:node-genes g)) @ep/visualize-genome-with) 
         (genome->graph g)
         :node->descriptor (fn [x]
                             (case x
                               :node-gene {:shape "record"
                                           :label (node-genes->records (:node-genes g))}
                               :conn-gene {:shape "record"
                                           :label (conn-genes->records (:connection-genes g))}
                               (into {:label (str x),
                                      :style "filled",
                                      :fixedsize "true"
                                      :shape "circle"
                                      :width "0.4"
                                      :height "0.4"}
                                     (case (:type (nth (:node-genes g) (dec x)))
                                       :input [[:fillcolor "#55ff55"][:rank "source"] [:pos (format "%d.0,0.0!" (int (+ (dec x)
                                                                                                                        (if (< input-cnt output-cnt)
                                                                                                                          (float (/ (Math/abs (- input-cnt output-cnt)) 2))
                                                                                                                          0.0))))]
                                               ]
                                       :bias [[:fillcolor  "#aaffff"][:rank "source"] [:pos (format "%d.0,0.0!" (int (+ (dec x)
                                                                                                                        (if (< input-cnt output-cnt)
                                                                                                                          (float (/ (Math/abs (- input-cnt output-cnt)) 2))
                                                                                                                          0.0))))]
                                              ]
                                       :hidden [[:fillcolor "gray"]]
                                       :output [[:fillcolor  "#ff5555"][:rank "sink"] [:pos (format "%d.0,10.0!" (int (+ (- x (inc input-cnt))
                                                                                                                         (if (> input-cnt output-cnt)
                                                                                                                           (float (/ (Math/abs (- input-cnt output-cnt)) 2))
                                                                                                                           0.0))))] 
                                                ]
                                       [[:filcolor "white"]]))))
         :edge->descriptor (fn [in out]
                             (case in
                               :node-gene {:style "invis"}
                               (weight-to-prop (weights [in out]) wr)))
         :node->cluster {:node-gene :gene,
                         :conn-gene :gene}
         
         :cluster->descriptor (fn [cluster]
                                (case cluster
                                  :gene {:style "invis"}
                                  {:style "invis"}))

         :options {:splines "true"
                   :dpi "50"
                   :rankdir "BT"
                   :mode "hier"
                   :layout "fdp"
                   :overlap "scale"
                   })))


(defn- flatten* [x]
  (filter #(and % (not (sequential? %)))
          (rest (tree-seq sequential? seq x))))

(defn- gen-connections
  [inputs outputs]
  (let [[connset res] (let [maxn (max (count inputs)
                                      (count outputs))
                            outs (shuffle outputs)]
                        (loop [i            0
                               [in & inr]   inputs
                               [out & outr] outs
                               connset      #{}
                               res          []]
                          (if (< i maxn)
                            (recur (inc i)
                                   (or inr inputs)
                                   (or outr outs)
                                   (conj connset [(:id in) (:id out)])
                                   (let [gene (gene/->Connection-gene
                                               (:id in) (:id out)
                                               (ep/rand-weight) true
                                               (or (@ep/gene-pool [(:id in) (:id out)])
                                                   (swap! ep/innovation-number inc)))]
                                     (swap! ep/gene-pool assoc [(:id in) (:id out)] (:innov gene))
                                     (conj res gene)))
                            [connset res])))]
    (vec (flatten* (conj res (for [in (shuffle inputs)
                                   out (shuffle outputs)]
                               (if (and (< (rand) @ep/connection-density)
                                        (not (connset [(:id in) (:id out)])))
                                 (let [gene (gene/->Connection-gene (:id in) (:id out) (ep/rand-weight) true 
                                                                    (or (@ep/gene-pool [(:id in) (:id out)])
                                                                        (swap! ep/innovation-number inc)))]
                                   (swap! ep/gene-pool assoc [(:id in) (:id out)] (:innov gene))
                                   gene))))))))

(defn initial-genome
  [input-count output-count]
  (let [tmp  (+ 2 input-count)
        inputs (into [(gene/->Node-gene 1 :bias)]
                     (mapv #(gene/->Node-gene % :input) (range 2 tmp)))
        nodes (into inputs
                    (mapv #(gene/->Node-gene (+ % tmp) :output)
                          (range output-count)))
        innov (atom 0)
        connections (gen-connections inputs (nthrest nodes (dec tmp)))]
    (->Genome nodes (vec (sort-by :innov connections)))))

(defn match-genes
  [^Genome g1 ^Genome g2]
  (loop [i1 (first (:connection-genes g1))
         i2 (first (:connection-genes g2))
         r1 (next (:connection-genes g1))
         r2 (next (:connection-genes g2))
         res []]
    (if (or i1 i2)
      (cond
       (or (not (and i1 i2)) (= (:innov i1) (:innov i2))) (recur (first r1)
                                                                 (first r2)
                                                                 (next r1)
                                                                 (next r2)
                                                                 (conj res [i1 i2]))
       (> (:innov i1) (:innov i2))  (recur i1
                                           (first r2)
                                           r1
                                           (next r2)
                                           (conj res [nil i2]))
       (< (:innov i1) (:innov i2))  (recur (first r1)
                                           i2
                                           (next r1)
                                           r2
                                           (conj res [i1 nil])))
      res)))

(defn- excess
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
                  (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (> (:innov (or x y)) minG)) matchG))))

(defn- excess-count
  [^Genome g1 ^Genome g2]
  (count (excess g1 g2)))

(defn- disjoint
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
               (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (and (not (and x y))
                                                       (<= (:innov (or x y)) minG)))
                                       matchG))))

(defn- disjoint-count
  [^Genome g1 ^Genome g2]
  (count (disjoint g1 g2)))

(defn- weight-diff
  [^Genome g1 ^Genome g2]
  (let [matches (filter (fn [[x y]]
                          (and x y))
                        (match-genes g1 g2))
        cnt (count matches)]
    (if (= cnt 0)
      0
      (/ (reduce + (map (fn [[x y]]
                          (Math/abs (- (:weight x) (:weight y)))) matches))
         cnt))))

(defn delta
  [^Genome g1 ^Genome g2]
  (+ (/ (+ (* (excess-count g1 g2) @ep/c1)
           (* (disjoint-count g1 g2) @ep/c2))
        (max (count (:connection-genes g1))
             (count (:connection-genes g2))))
     (* (weight-diff g1 g2) @ep/c3)))




