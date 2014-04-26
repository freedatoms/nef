(ns nef.utils)

(defn humanize-keyword 
  "Make keyword into word"
  [keyword]
  (-> (str keyword)
      (clojure.string/replace #"[-:]" " ")
      clojure.string/trim
      clojure.string/capitalize))

(defn ?-> 
  "If cond then apply fun to in otherwise return in."
  [cond fun in]
  (if cond
    (fun in)
    in))

(defn max-pos
  "Return position of maximal item."
  [v]
  (loop [[x & xs] v
         max      0
         ith      0
         i        0]
    (if x
      (if (> x max)
        (recur xs x  i    (inc i))
        (recur xs max ith (inc i)))
      ith)))


(defn pair-wise-reduce [fun init-acc xs ys]
  (loop [[x & xx] xs
         [y & yy] ys
         acc      init-acc]
    (if (and xx yy)
      (recur xx yy (fun acc x y))
      (fun acc x y))))
