(ns nef.neprotocol)

(defprotocol NeuroEvolution
  (run [this] [this run-params] "Runs NeuroEvolutionary algorithm")
  (save-log-to-csv [this filename] "Saves log to csv")
  (show-log [this] "Shows log")
  (plot [this] [this [x-label y-label] xy-pairs] "Plots data from log")
  (get-log [this] "Returns log as incanter data set"))

(defn humanize-keyword [keyword]
  (-> (str keyword)
      (clojure.string/replace #"[-:]" " ")
      clojure.string/trim
      clojure.string/capitalize))

(defn make-classification-of 
  [name dataset input-cols output-cols &{:keys [decrease-outputs]}]
  {:type :classification,
   :name name,
   :data dataset,
   :input-cols input-cols,
   :output-cols output-cols,
   :decrease-outputs decrease-outputs})

(defn make-regression-of 
  [name dataset input-cols output-cols]
  {:type :regression,
   :name name,
   :data dataset,
   :input-cols input-cols,
   :output-cols output-cols})

(defn make-maze 
  []
  {:type :maze,
   :name "Maze"})

(defn make-use-of-custom-fitness 
 "Takes name and unary function which returns fitness function,
  the only argument for this function is function that evaluates 
  neural network. Fitness function takes one argument - individual.
  Function that evaluates neural network takes individual and input."
  [name fitness-function]
  {:type :custom-fitness,
   :name name,
   :fitness-function fitness-function})

(defn min-max-normalize-column [col]
  (let [min (apply min col)
        max (apply max col)
        diff (- max min)]
    (mapv (fn [x] (/ (- x min) diff)) col)))
