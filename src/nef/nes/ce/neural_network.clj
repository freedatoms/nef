(ns nef.nes.ce.neural-network
  (:import clojure.lang.PersistentQueue
           nef.nes.ce.java.Cell
           java.util.LinkedList
           [javax.swing JFrame ImageIcon])
  (:require [rhizome
             [viz :as viz]
             [dot :as dot]])
  (:use nef.graphviz-enabled))



(set! *warn-on-reflection* true)

(defn- -do-net-function [fun net]
  (let [nodes (into (keys (:neurons net)) ["input"])
        adj (into (zipmap nodes (map  #(map first (:out ((:neurons net) %))) nodes))
                  {"input" (:input net)})
        output-node (first (first (:out ((:neurons net) (first (:output net))))))]
    (fun
     nodes
     adj
     :options {:dpi *graphviz-dpi*}
     :edge->descriptor (fn [from to]
                         (if (= from "input")
                           (let [w (nth (:input-weights net) (.indexOf ^clojure.lang.PersistentVector (:input net) ^java.lang.String to))]
                             (if (< w 0)
                               {:color :red :label (if *show-edge-label* w "")}
                               {:color :black :label (if *show-edge-label* w "")}))
                           (let [[[_ w]] (filter #(= to (first %)) (:out ((:neurons net) from) [[to 1]]))]
                             (if (< w 0)
                               {:color :red :label (if *show-edge-label* w "")}
                               {:color :black :label (if *show-edge-label* w "")}))))
     :node->descriptor (fn [n]
                         (cond
                          (= n "input") {:shape :box :label "INPUT"}
                          (= n output-node) {:shape :box :label "OUTPUT"}
                          :else {:shape :circle
                                 :label (if *show-node-label* (:bias ((:neurons net) n)) "")}) ))))

(defrecord Neural-Net
    [input input-weights output neurons]
  GraphvizEnabled
  (save-image [this filename]
    (viz/save-image (-do-net-function
                     viz/graph->image
                     this)
                    filename))
  (save-dot [this filename]
    (spit filename
          (-do-net-function dot/graph->dot this)))
  (view [this]
    (view this (viz/create-frame "Neural Net")))
  (view [this frame]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setImage image-icon (-do-net-function viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true))))
  (view [this frame title]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setTitle frame title)
      (.setImage image-icon (-do-net-function viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true)))))

(defrecord Neuron
    [in out activation-function bias])

(defn topsort
  [network]
  (let [state  (atom (transient {}))
        sorted (atom (transient []))
        graph (:neurons network)]
    (letfn [(order [n]
              (swap! state #(assoc! % n :open))
              (doall  (doseq [v (:in (graph n))]
                        (cond
                         (= (@state v :fresh) :fresh) (order v)
                         (= (@state v :fresh) :open) (throw "graf je cyklicky"))))
              (swap! state #(assoc! % n :closed))
              (swap! sorted #(conj! % n)))]
      (doall (doseq [n (:output network)]
               (order n)))
      (persistent! @sorted))))

(defn evaluate-ff-net
  "Evaluates feed-forward neural network given inputs"
  ([network inputs] (evaluate-ff-net network inputs (topsort network)))
  ([network inputs sorted-nodes]
     (let [m (atom (transient (zipmap (:input network)
                                      (mapv * inputs (:input-weights network)))))]
       (doall (doseq [n sorted-nodes]
                (let [neuron ((:neurons network) n)
                      val ((:activation-function neuron) neuron (@m n 0))]
                  (doall (doseq [[ch w] (:out neuron)]
                           (swap! m #(assoc! % ch (+ (% ch 0) (* val w))))))
                  (swap! m #(assoc! % n val)))))
       (mapv  @m (:output network)))))




(defn- get-cell-name [^Cell cell]
   (second (re-seq #"[^@]+" (.toString cell))))

(defn- graph->map
  "Takes the structure from evaluate-tree-grammar and DFSs trought it"
  [[input output]
   & {:keys [children keyfn valfn]
      :or {children #(.getOut ^Cell %),
           keyfn get-cell-name,
           valfn (fn [^Cell cell]
                   {:out (map get-cell-name (.getOut cell))
                    :in  (zipmap
                          (map get-cell-name (.getIn cell))
                          (.getInWeights cell))
                    :bias (.getBias cell)})}}]
  (loop [vertices (transient {})
         explored #{input}
         frontier [input]]
    (if (empty? frontier)
      (persistent! vertices)
      (let [v (peek frontier)
            ch (children v)]
        (recur
         (assoc! vertices  (keyfn v) (valfn v))
         (into explored ch)
         (into (pop frontier) (remove explored ch)))))))

(defn get-weights-for-output [^Cell cell]
  (mapv (fn  [^Cell x]
          (.get
           (.getInWeights x)
           (.indexOf (.getIn x) cell))) (.getOut cell)))

(defn graph->neural-net
  "Takes result of evaluate-tree-grammar and transforms it into neural net"
  [[^Cell input ^Cell output :as graph]]
  
  (map->Neural-Net {:input (mapv get-cell-name (.getOut input))
                    :input-weights (get-weights-for-output input)
                    :output (mapv get-cell-name (.getIn output))
                    :neurons (dissoc (graph->map graph
                                                 :valfn (fn [^Cell cell]
                                                          (->Neuron
                                                           (if ((into #{} (.getOut input)) cell)
                                                             []
                                                             (mapv get-cell-name (.getIn cell)))
                                                           (mapv (fn [x y]
                                                                   [(get-cell-name x) y])
                                                                 (.getOut cell)
                                                                 (get-weights-for-output cell))
                                                           (.getActivationFunction cell)
                                                           (.getBias cell))))
                                     (get-cell-name input))}))



(defn evaluate-ff-net-cell
  [[^Cell input ^Cell output gr] inputs]
  (let [neurons (atom {})
        queue (LinkedList.)
        inputs* (into (vec inputs) (repeat (- (count (.getOut ^Cell input))
                                        (count inputs))
                                     0))]
    (dorun (mapv (fn [^Cell x y]
                   (.add queue x)
                   (swap! neurons #(assoc % (.toString x)
                                          {:ins (* y (first (.getInWeights x)))
                                           :realized 1}))) (.getOut input) inputs*))
    (while (.peek queue)
      (let [^Cell cur (.poll queue)
            val ((.getActivationFunction cur) cur (:ins (@neurons (.toString cur))))]
        (doseq [^Cell ch (.getOut cur)]
          (let [m (@neurons (.toString ch))]
            (swap! neurons #(assoc % (.toString ch)
                                   (assoc m
                                     :realized (inc (:realized m 0))
                                     :ins (+ (:ins m 0)
                                             (* val
                                                (.get (.getInWeights ch)
                                                      (.indexOf (.getIn ch) cur)))))))
            (if (= (count (.getIn ch)) (:realized (@neurons (.toString ch))))
              (.add queue ch))))
        (swap! neurons #(assoc % (.toString cur) (assoc (% (.toString cur)) :ins val)))))
    (mapv (fn [^Cell x]
            (when-not (= (count (.getIn x)) (:realized (@neurons (.toString x))))
              
              (view gr)
              (view (graph->neural-net [input output]))              (throw "warning"))
            (:ins (@neurons (.toString x))))
          (.getIn output))))


