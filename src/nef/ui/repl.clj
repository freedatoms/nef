(ns nef.ui.repl
  (:require [nef.nes
             [es :as es]
             [ce :as ce]
             [neat :as neat]]
            [nef.nes.neat
             [gui :as neatgui]]
            [nef
             [problem-domain :as pd]]
            [incanter
             [core :as c]
             [io :as io]])
  (:use nef.neprotocol)
  (:import java.io.File))

(def registered-algorithms
  (atom ["neat" "ce" "es"]))

(defn- escape [s]
  (if (char? s)
    (pr-str s)
    s))

(defn- read-option [prompt options]
  (println prompt)
  (loop [[opt & opts] options
         i 1]
    (when opt
      (println "\t\t" (str i "] ") (escape opt))
      (recur opts (inc i))))
  (loop [line (read-line)]
    (when line
      (let [rl (read-string line)]
        (if (and (number? rl)
                 (> rl 0)
                 (<= rl (count options)))
          (nth options (dec rl))
          (recur (read-line)))))))

(defn- read-bool [prompt]
  (println prompt "[y/n]")
  (loop [line (read-line)]
    (when line
      (case (clojure.string/trim-newline line)
        "y" true
        "yes" true
        "n" false
        "no" false
        (recur (read-line))))))

(defn- read-function [prompt]
  (println prompt)
  (eval (read)))

(defn- read-a-string [prompt]
  (println (str prompt ":"))
  (clojure.string/trim-newline (read-line)))

(defn- read-a-filename [prompt]
  (println (str prompt ":"))
  (let [fn (clojure.string/trim (read-line))
        f (File. fn)]
    (try
      (.getCanonicalPath f)
      fn
      (catch Exception e
          (read-a-filename prompt)))))

(defn- read-a-number [prompt]
  (println (str prompt ":"))
  (loop [num (read-string (read-line))]
    (if (number? num)
      num
      (recur (read-string (read-line))))))

(defn- customize-classification []
  (println)
  (println "== Customize Classification ==")
  (let [experiment-name (read-a-string "Enter experiment name")
        dataset-filename (read-a-filename "Enter data set filename")
        delimeter (read-option "Choose a delimeter:"
                               [\, \  \; \tab])
        input-cols (read-string
                    (read-a-string "Select input columns: (use vector notation (e.g., [1 2 3 4]))"))
        output-col (vector (read-string (read-a-string "Select output column (i.e., column containing labels)")))
        normalize-inputs (read-bool "Normalize inputs?")
        decrease-output (read-bool "Is label counted from 1? (If the label is non-numeric press 'n')")]
    (let [pd (pd/make-classification :experiment-name experiment-name
                                     :dataset (io/read-dataset dataset-filename
                                                               :delim delimeter)
                                     :input-cols input-cols
                                     :output-col output-col
                                     :normalize-inputs normalize-inputs
                                     :decrease-output decrease-output)]
      (if (read-bool "Review dataset? (requires swing)")
        (do (c/view (:dataset pd))
            (println "Note that no action implied from previous questions has been applied yet.")
            (if (read-bool "Is it imported incorrectly? Try again?")
              (recur)
              pd))
        pd))))

(defn- customize-regression []
  (println)
  (println "== Customize Regression ==")
  (let [experiment-name (read-a-string "Enter experiment name")
        dataset-filename (read-a-filename "Enter data set filename")
        delimeter (read-option "Choose a delimeter:"
                               [\, \  \; \tab])
        input-cols (read-string
                    (read-a-string "Select input columns: (use vector notation (e.g., [1 2 3 4]))"))
        output-cols (read-string (read-a-string "Select output columns (columns with predicted values)"))
        normalize-inputs (read-bool "Normalize inputs?")
        normalize-outputs (read-bool "Normalize outputs?")
        ]
    (let [pd (pd/make-regression :experiment-name experiment-name
                                 :dataset (io/read-dataset dataset-filename
                                                           :delim delimeter)
                                 :input-cols input-cols
                                 :output-cols output-cols
                                 :normalize-inputs normalize-inputs
                                 :normalize-outputs normalize-outputs)]
      (if (read-bool "Review dataset? (requires swing)")
        (do (c/view (:dataset pd))
            (println "Note that no action implied from previous questions has been applied yet.")
            (if (read-bool "Is it imported incorrectly? Try again?")
              (recur)
              pd))
        pd))))

(defn- customize-maze []
  (println)
  (println "== Customize Maze ==")
  (let [normalize-inputs (read-bool "Normalize inputs?")
        save-images  (read-bool "Save maze images?")]
    (pd/make-maze :normalize-inputs normalize-inputs
                  :save-images save-images
                  :filename-format
                  (when save-images
                    (read-a-string (str "Enter a c-like format string to describe"
                                        "a file names for images of maze. First param "
                                        "is number of current generation and the second "
                                        "is fitness of the best individual showed in the maze."
                                        " For example \"/tmp/maze/generation-%d-fitness-%f.png\"."))))))


(defn- customize-problem-domain []
  (println)
  (println "== Customize Problem Domain ==")
  (let [name (read-a-string "Enter name of problem domain:")
        experiment-name (read-a-string "Enter experiment name:")
        fitness (read-function (str "Enter function returning fitness function:\n "
                                    " (f this evaluator) -> (f individual) -> number :"))
        do-with-best (read-function (str "Enter function that is run with best individual"
                                         " from each generation.  (e.g., can be used to show a maze)\n"
                                         "(f this evaluator) -> (f best-individual generation"
                                         " fitness-value) -> nil :"))]
    (pd/->CustomFitness name experiment-name fitness do-with-best)))


(defn- customize-ce []
  {};todo
  )

(defn- customize-es []
  {:mu (read-a-number "Mu - number of individuals in generation")
   :rho (read-a-number "Rho - number of parents")
   :lambda (read-a-number "Lambda - number of offspring")
   :topology (read-a-string "Enter a vector describing topology, e.g., [5 3 2] for 5 input neurons, 3 in hidden layer and 2 in output layer.")
   :commaSelection (read-bool "Do you wish to use Comma Selection?")})

(defn- customize-neat []
  (neatgui/show-options)
  (neatgui/get-settings))

(defmulti dispatch vector)

(defmethod dispatch ["ce" "Classification" "iris"] [_ _ _]
  (ce/make-iris-classification))
(defmethod dispatch ["es" "Classification" "iris"] [_ _ _]
  (es/make-iris-classification))
(defmethod dispatch ["neat" "Classification" "iris"] [_ _ _]
  (neat/make-iris-classification))

(defmethod dispatch ["ce" "Classification" "wine"] [_ _ _]
  (ce/make-wine-classification))
(defmethod dispatch ["es" "Classification" "wine"] [_ _ _]
  (es/make-wine-classification))
(defmethod dispatch ["neat" "Classification" "wine"] [_ _ _]
  (neat/make-wine-classification))

(defmethod dispatch ["ce" "Classification" "glass"] [_ _ _]
  (ce/make-glass-classification))
(defmethod dispatch ["es" "Classification" "glass"] [_ _ _]
  (es/make-glass-classification))
(defmethod dispatch ["neat" "Classification" "glass"] [_ _ _]
  (neat/make-glass-classification))

(defmethod dispatch ["ce" "Classification" "Custom"] [_ _ _]
  (ce/make-ce (customize-classification) (customize-ce)))
(defmethod dispatch ["es" "Classification" "Custom"] [_ _ _]
  (es/make-es (customize-classification) (customize-es)))
(defmethod dispatch ["neat" "Classification" "Custom"] [_ _ _]
  (neat/make-neat (customize-classification) (customize-neat)))

(defmethod dispatch ["ce" "Regression" "Yacht Hydrodynamics"] [_ _ _]
  (ce/make-yacht-regression))
(defmethod dispatch ["es" "Regression" "Yacht Hydrodynamics"] [_ _ _]
  (es/make-yacht-regression))
(defmethod dispatch ["neat" "Regression" "Yacht Hydrodynamics"] [_ _ _]
  (neat/make-yacht-regression))


(defmethod dispatch ["ce" "Regression" "Custom"] [_ _ _]
  (ce/make-ce (customize-regression) (customize-ce)))
(defmethod dispatch ["es" "Regression" "Custom"] [_ _ _]
  (es/make-es (customize-regression) (customize-es)))
(defmethod dispatch ["neat" "Regression" "Custom"] [_ _ _]
  (neat/make-neat (customize-regression) (customize-neat)))

(defmethod dispatch ["ce" "Maze" "maze"] [_ _ _]
  (ce/make-maze-reinforcement-learning))
(defmethod dispatch ["es" "Maze" "maze"] [_ _ _]
  (es/make-maze-reinforcement-learning))
(defmethod dispatch ["neat" "Maze" "maze"] [_ _ _]
  (neat/make-maze-reinforcement-learning))


(defmethod dispatch ["ce" "Maze" "Custom"] [_ _ _]
  (ce/make-ce (customize-maze) (customize-ce)))
(defmethod dispatch ["es" "Maze" "Custom"] [_ _ _]
  (es/make-es (customize-maze) (customize-es)))
(defmethod dispatch ["neat" "Maze" "Custom"] [_ _ _]
  (neat/make-neat (customize-maze) (customize-neat)))


(defmethod dispatch ["ce" "Custom" nil] [_ _ _]
  (ce/make-ce (customize-problem-domain) (customize-ce)))
(defmethod dispatch ["es" "Custom" nil] [_ _ _]
  (es/make-maze-reinforcement-learning))
(defmethod dispatch ["neat" "Custom" nil] [_ _ _]
  (neat/make-maze-reinforcement-learning))


(defn- run-loop [ne]
  (let [option (read-option "Choose what to do:" ["Save the log to a csv file"
                                                  "Show the log"
                                                  "Plot the log"
                                                  "quit"])]
    (case option
      "Save the log to a csv file" (save-log-to-csv ne (read-a-filename "File name: (e.g., /tmp/mylog.csv)"))
      "Show the log" (show-log ne)
      "Plot the log" (plot ne) ;; TODO: options
      "quit" (do
               (println "Quitting...")
               (System/exit 0)))
    (recur ne)))


(defn repl
  []
  (println
   (clojure.string/join
    "\n"
    ["Welcome to Neuroevolutionary Framework"
     ""
     (str "Currently there are " (count @registered-algorithms)
          " registered neuroevolutionary algorithms.")
     ""]
    ))
  (let [alg (read-option "Choose one from:"
                         @registered-algorithms)
        type (read-option "You can choose one of the following problem domains:"
                          ["Classification", "Regression", "Maze", "Custom"])
        ne (case type
             "Classification" (dispatch alg type (read-option "Choose one of the following:"
                                                              ["iris" "wine" "glass" "Custom"]))
             "Regression" (dispatch alg type (read-option "Choose one of the following:"
                                                          ["Yacht Hydrodynamics" "Custom"]))
             "Maze" (dispatch alg type (read-option "Choose one of the following:"
                                                    ["maze" "Custom"]))
             (dispatch alg type nil))]
    (run ne) ;; TODO: Let choose run params
    (run-loop ne)))

