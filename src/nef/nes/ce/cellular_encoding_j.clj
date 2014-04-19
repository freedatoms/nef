(ns nef.nes.ce.cellular-encoding-j
  (:require [clojure
             [zip :as z]]
            [nef.nes.ce
             [neural-network :as nn]])
  (:import nef.nes.ce.java.Cell
           java.util.LinkedList))

(set! *warn-on-reflection* true)

(defn act-fun1
  "As described in cellular encoding (p. 7)"
  [neuron sum]
  (if (> sum (.bias ^Cell neuron))
    1
    0))

(defn act-tanh
  [neuron sum]
  (Math/tanh (+ sum (.bias ^Cell neuron))))

(defn act-sgn
  [neuron sum]
  (Math/signum (+ sum (.bias ^Cell neuron))))

(defn act-sigmoid
  [neuron sum]
  (/ 1 (+ 1 (Math/exp (- (+ sum (.bias ^Cell neuron)))))))

(defn act-gaussian
  [neuron sum]
  (Math/exp (- (Math/pow (+ sum (.bias ^Cell neuron)) 2))))

(defn act-sin
  [neuron sum]
  (Math/sin (+ (.bias ^Cell neuron) sum)))

(def ^:dynamic *lifespan* "Lifespan of an ancestral cell" 2)
(def ^:dynamic *default-activation-function* "Default activation function" act-fun1)

(defn init-cell [& {:keys [in inweight out lr bias gr life act-fun]
                    :or {in [], inweight [], out [], lr 0, bias 0.0,
                         life *lifespan*,
                         act-fun *default-activation-function*}}]
  (Cell. (LinkedList. out) (LinkedList. in) (LinkedList. inweight) lr bias life gr act-fun))

(definline left-subtree [^Cell cell]
  `(z/down (z/right (.getGrammarRoot ~cell))))

(definline right-subtree [^Cell cell]
  `(z/down (z/right (z/right (.getGrammarRoot ~cell)))))

(defn evaluate-tree-grammar [grammar]
  (let [queue (LinkedList.)
        down (fn [^Cell cell]
               (.setGrammarRoot cell (left-subtree cell))
               (.add queue cell))
        ^Cell input (init-cell)
        ^Cell initial-cell (init-cell :in [input]
                                      :inweight [1.0]
                                      :gr (z/down (z/seq-zip grammar)))
        ^Cell output (init-cell :in [initial-cell]
                                :inweight [1.0])
        ]
    (.setOut input (LinkedList. [initial-cell]))
    (.setOut initial-cell (LinkedList. [output]))
    (.add queue initial-cell)
    (while (.peek queue)
      (let [^Cell cell (.poll queue)]
        (case (z/node (.getGrammarRoot cell))
          seq (do
                (.add queue cell)
                (.add queue (.handleSeq cell
                                        (left-subtree cell)
                                        (right-subtree cell))))
          par (do
                (.add queue cell)
                (.add queue (.handlePar cell
                                        (left-subtree cell)
                                        (right-subtree cell))))
          wait (down cell)
          val- (do
                 (.setCurrentInLinkWeight cell -1)
                 (down cell))
          val+ (do
                 (.setCurrentInLinkWeight cell 1)
                 (down cell))
          doubleval (do
                      (.multiplyCurrentInLinkWeight cell 2)
                      (down cell))
          mult-1val (do
                      (.multiplyCurrentInLinkWeight cell -1)
                      (down cell))
          mult10val (do
                      (.multiplyCurrentInLinkWeight cell 10)
                      (down cell))
          mult0.1val (do
                       (.multiplyCurrentInLinkWeight cell 0.1)
                       (down cell))
          mult1.25val (do
                        (.multiplyCurrentInLinkWeight cell 1.25)
                        (down cell))
          mult0.8val (do
                       (.multiplyCurrentInLinkWeight cell 0.8)
                       (down cell))
          halfval (do
                    (.multiplyCurrentInLinkWeight cell 0.5)
                    (down cell))
          addval (do
                   (.addCurrentInLinkWeight cell 1)
                   (down cell))
          subval (do
                   (.addCurrentInLinkWeight cell -1)
                   (down cell))
          rec  (do
                 (.decLife cell)
                 (when (> (.getLife cell) 0)
                   (.setGrammarRoot cell (z/down (z/seq-zip grammar)))
                   (.add queue cell)))
          decbias (do
                    (.setBias cell 0)
                    (down cell))
          incbias (do
                    (.setBias cell 1)
                    (down cell))
          halfbias (do
                     (.setBias cell (/ (.getBias cell) 2))
                     (down cell))
          doublebias (do
                       (.setBias cell (* (.getBias cell) 2))
                       (down cell))
          mult-1bias (do
                       (.setBias cell (* (.getBias cell) -1))
                       (down cell))
          mult10bias (do
                       (.setBias cell (* (.getBias cell) 10))
                       (down cell))
          mult0.1bias (do
                        (.setBias cell (* (.getBias cell) 0.1))
                        (down cell))
          mult1.25bias (do
                         (.setBias cell (* (.getBias cell) 1.25))
                         (down cell))
          mult0.8bias (do
                        (.setBias cell (* (.getBias cell) 0.8))
                        (down cell))

          addbias (do
                    (.setBias cell (+ (.getBias cell) 1))
                    (down cell))
          subbias (do
                    (.setBias cell (- (.getBias cell) 1))
                    (down cell))
          end   (.setGrammarRoot cell nil)
          blife (do
                  (if (> (.getLife cell) 1)
                    (down cell)
                    (do
                      (.setGrammarRoot cell (right-subtree cell))
                      (.add queue cell))))
          inclr (do
                  (.inclr cell)
                  (down cell))
          declr (do
                  (.declr cell)
                  (down cell))
          cut   (do
                  (.cutCurrentInLink cell)
                  (down cell))
          acttanh (do
                    (.setActivationFunction cell act-tanh)
                    (down cell))
          actsgn (do
                   (.setActivationFunction cell act-sgn)
                   (down cell))
          actsigmoid (do
                       (.setActivationFunction cell act-sigmoid)
                       (down cell))
          actsin (do
                   (.setActivationFunction cell act-sin)
                   (down cell))
          actgauss (do
                     (.setActivationFunction cell act-gaussian)
                     (down cell))
          )))
    [input output]
    ))

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
         (assoc! vertices (keyfn v) (valfn v))
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
  (nn/map->Neural-Net {:input (mapv get-cell-name (.getOut input))
                       :input-weights (get-weights-for-output input)
                       :output (mapv get-cell-name (.getIn output))
                       :neurons (dissoc (graph->map graph
                                                    :valfn (fn [^Cell cell]
                                                             (nn/->Neuron
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

(defn ce->neural-net
  "Evaluates tree grammar and returns neural-net"
  [grammar]
  (graph->neural-net (evaluate-tree-grammar grammar)))

