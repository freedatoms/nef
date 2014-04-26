(ns nef.neprotocol)

(defprotocol NeuroEvolution
  (run [this] [this run-params] "Runs NeuroEvolutionary algorithm")
  (save-log-to-csv [this filename] "Saves log to csv")
  (show-log [this] "Shows log")
  (plot [this] [this [x-label y-label] xy-pairs] "Plots data from log")
  (get-log [this] "Returns log as incanter data set"))
