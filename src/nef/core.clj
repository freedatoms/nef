(ns nef.core
  (:gen-class)
  (:require [nef
             [problem-domain :as pd]]
            [nef.ui 
             [repl :as repl]]))

(defn -main 
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (if (and (= 1 (count args))
           (#{\/ \\} (last (first args))))
    (do
      (println "Setting data set path to:" (first args))
      (reset! pd/dataset-prefix (first args))
      (try
        (repl/repl)
        (catch Throwable e
          (println e)
          (System/exit 1))))
    (println "Usage: java -jar nef.jar <path to dataset folder>/")))
