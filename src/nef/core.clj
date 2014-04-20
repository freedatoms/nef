(ns nef.core
  (:gen-class)
  (:require [nef.nes 
             [neat :as neat]])
  (:use incanter.core
        nef.neprotocol))

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
  (dotimes [x 5]
    (let [maze (neat/make-maze-reinforcement-learning)]
      (run maze)
      (dotimes [g 100]
        (spit (str "/home/frydatom/neat/gen-" g "-" x "-" (java.util.Date.)) (str ($ :performance ($where {:generation g} (get-log maze))) "\n"))))))
