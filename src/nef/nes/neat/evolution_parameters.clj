(ns nef.nes.neat.evolution-parameters)

(defrecord Option
    [id
     name
     var
     description 
     type
     value])

(def options (atom []))

(defmacro defoption
  "Creates an option"
  [name description default-value 
   &{:keys [type value title]
     :or {type :float}}]
  (let [titl (or title (str name))
        val  (or value (case type
                         :probability [0.0 1.0]
                         nil))
        typ (case type
              :probability :float
              type)]
    `(do
       (def ~name ~description (ref ~default-value))
       (swap! options conj (Option. ~(keyword (str name)) ~titl ~name ~description ~typ ~val)))))


;; Evolution parameters
(defoption population-size
  "Size of population"
  150
  :type :int
  :value [1])

(defoption generation-count
  "Maximum number of generations."
  100
  :type :int
  :value [1])

(defoption input-count
  "Number of inputs"
  2
  :type :int
  :value [1])

(defoption output-count
  "Number of outputs"
  1
  :type :int
  :value [1])



(defoption c1
  "Excess gene importance in delta function"
   1.0
  :type :float)
(defoption c2
  "Disjoint gene importance in delta function"
   1.0
  :type :float)
(defoption c3
  "Weight difference importance in delta function"
   0.4
  :type :float)
(defoption dt
  "Delta threshold for species separation"
   3.0
  :type :float)
(defoption transfer-fun
  "Activation(=tranfer) function"
   (fn [x] (/ 1 (+ 1 (Math/exp (* -4.9 x)))))
  :type :function
  :title "Transfer function")
(defoption survival-rate-in-species 
  "Specifies how many individui should survive the elimination 
   of the lowest performing members in every species."
   0.2
  :type :probability)
(defoption weight-range
  "Lower and upper bound used for generating new genomes and clamping"
  [-2.5 2.5]
  :type :range)
(defoption clamp-weight-factor
  "Multiplier for weight range for clamping the weight. Setting it to 0 disables the clamping"
  0.0
  :type :float)

;; Creation
(defoption connection-density
  "How many connections are created in initial population.
   If set to 0 only max(in+1,out) connections will be created"
   0.0
  :type :probability)
(defoption fitness-fun
  "Fitness function"
  (fn [&_] (throw (Exception. "Fitness function is not set")))
  :type :function)

;; Mutation
(defoption mutate-only-prob
  "Probability of mutation without crossover"
  0.25
  :type :probability)
(defoption mutation-prob
  "Probability of mutation"
  0.25
  :type :probability)
(defoption mutate-weights-prob
  "Probability of mutating weights"
  0.8
  :type :probability)
(defoption mutate-weights-perturb-prob
  "Probability of weight perturbation (= P(perturb|mutate weights))"
  0.9
  :type :probability)
(defoption mutate-weights-perturb-sigma
  "Sigma of mutate weights perturbation"
  2.5
  :type :float)
(defoption add-connection-prob
  "Probability of add-connection mutation"
  0.05
  :type :probability)
(defoption add-node-prob
  "Probability of add-node mutation"
  0.03
  :type :probability)


;; Crossover
(defoption crossover-prob
  "Probability of crossover"
  0.75
  :type :probability)
(defoption interspecies-mating-prob
  "Probability of interspecies mating"
  0.001
  :type :probability)
(defoption disable-in-crossover
  "Probability of being disabled when either parent gene is"
  0.75
  :type :probability)

;; Visualization
(defoption visualize-genome-with
  "Set to [] if you are only interested in shape of the neural net"
  [:conn-gene :node-gene]
  :type :any-of
  :value [:conn-gene :node-gene])


;; Druha verze
(defoption dt-delta
  "Used when targeting concrete number of species"
  0.4
  :type :float)

(defoption target-species
  "Target number of species. 0 for not adjusting dt."
  0
  :type :int
  :value [0])

(defoption young-age
  "When is species young"
  10
  :type :int
  :value [1])

(defoption young-age-multiplier
  "Multiply fitness by this of young species to aid it"
  1.2
  :type :float)
        
(defoption old-age
  "When is species old"
  30
  :type :int
  :value [1])

(defoption old-age-multiplier 
  "Multiply fitness by this to penalize old species"
  0.2
  :type :float)
        
(defoption stagnation-age
  "How long keep the stagnating species"
  15
  :type :int
  :value [1])

(defoption tournament-k
  "Used for pool size in tournament selection"
  1
  :type :int
  :value [1])

(defoption elitism
  "Copy best individual in each species?" 
  true
  :type :boolean)

(defoption min-elitism-size
  "How many individuals do the species need to copy the best individual to new generation"
  5
  :type :int
  :value [1])

;; Mating
(defoption mate-only-prob
  "Probability of crossover without mutation"
  0.2
  :type :probability)

(defoption mate-by-choosing-prob
  "Mate by choosing genes otherwise average the matching genes"
  0.6
  :type :probability)

   
;;; internal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def innovation-number
  "Used for historical markings"
  (atom 0))
(def species-count 
  "Used for numbering species"
  (atom 0))
(def individuals-count 
  "Used for numbering species"
  (atom 0))
(def gene-pool
  "used during add-node and add-connection mutations to enhance gene-matching"
  (atom {}))



;; Functions
(defn rand-weight
  "Random floating-point number from range specified in weight-range ref or in args"
  ([]
     (rand-weight (first @weight-range) (second @weight-range)))
  ([lo hi]
     (+ (rand (- hi lo)) lo)))


