(ns nef.nes.ce.evolution-parameters)

;; (defmacro ensure-set
;;   "Throws an error with message if not rebound to other value"
;;   [message]
;;   `(fn [& ~'_] (throw (Error. ~message))))

;; (def ^:dynamic *lifespan*
;;   "Lifespan of an ancestral cell"
;;   2)
;; (def ^:dynamic *default-activation-function*
;;   "Default activation function"
;;   (ensure-set "*default-activation-function* is not set"))

;; (def ^:dynamic *inputs*
;;   "Inputs for the neural net used by unary fitness function"
;;   [[0 0] [0 1] [1 0] [1 1]])
;; (def ^:dynamic *outputs*
;;   "Outputs for the neural net used by unary fitness function"
;;   [[0] [1] [1] [0]])

;; (def ^:dynamic *output-count*
;;   "Count of output neurons (sometimes it is impossible to deduce this information from *outputs*)"
;;   3)

;; (def ^:dynamic *fitness-function*
;;   "Fitness function for evolution"
;;   (ensure-set "*fitness-function* is not set"))

;; (def ^:dynamic *binary-function*
;;   ['seq 'par])
;; (def ^:dynamic *unary-function*
;;   ['val- 'incbias 'val+ 'decbias 'wait])
;; (def ^:dynamic *nullary-function*
;;   ['end 'rec])


;; (def ^:dynamic *genome-length*
;;   "The initial number of nodes in genome tree."
;;   10)

;; (def ^:dynamic *max-genome-length*
;;   "The maximum number of nodes in genome tree."
;;   20)

;; (def ^:dynamic *mutation-rate*
;;   "Mutation rate is probability that allele mutates"
;;   0.005)
