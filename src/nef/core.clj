(ns nef.core
  (:gen-class))

(defn- print-help []
  (println 
   (->>
    ["Help"
     "use run to run"]
    (clojure.string/join "\n"))))

(defmacro repl-recur []
  `(do (print ">> ")
       (flush)
       (recur (read-line))))

(defn- repl []
  (print ">> ")
  (flush)
  (loop [line (read-line)]
    (case line 
      "help" (do  (print-help)
                  (repl-recur))
      "run"  (println "running")
      nil     (println "exitting...")
      (do (println "Unknown command")
          (print-help)
          (repl-recur)))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "NeuroEvolution Framework")
  (repl))
