(ns nef.preprocessing)

(defn min-max-normalize [[lower-bound upper-bound]]
  (let [range (- upper-bound lower-bound)]
    (fn [col]
      (let [min (apply min col)
            max (apply max col)
            diff (- max min)]
        (mapv (fn [x] (double (+ lower-bound 
                                 (* range (/ (- x min) diff))))) 
              col)))))

(defn min-max-normalize-columns [[lower-bound upper-bound]]
  (let [normalize-column (min-max-normalize [lower-bound upper-bound])]
    (fn [cols]
      (if (coll? (first cols))
        (let [cls (loop 
                      [[x & xs] (rest cols)
                       cs (mapv vector (first cols))]
                    (if xs
                      (recur xs (mapv conj cs x))
                      (mapv conj cs x)))]
          (apply mapv (fn [& xs] (vec xs))
                 (mapv normalize-column cls)))
        (mapv vector (normalize-column cols))))))

