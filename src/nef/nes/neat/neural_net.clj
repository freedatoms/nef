(ns nef.nes.neat.neural-net
  (:use [nef.nes.neat
         evolution-parameters]))

(set! *warn-on-reflection* true)

(defn- doubles-map-to-with-diff
  [fun ^doubles from ^doubles to
   & {:keys [starting-index]
      :or {starting-index 0}}]
  (let [len (alength from)]
    (loop [idx (int starting-index)
           diff (double 0.0)]
      (if (< idx len)
        (let [^double nd (fun (aget from idx))
              d ^double (Math/abs ^double (- ^double (aget to idx) ^double nd))]
          (aset to idx nd)
          (aset from idx 0.0)
          (recur (inc idx)
                 ^double (+ d diff)))
        diff))))

(defn evaluate-neural-net-with-activation-cycles
  [genome inputs activation-cycles & {:keys [threshold]
                                      :or {threshold 0.00001}}]
  (let [output-index (count (filter #(#{:input :bias} (:type %)) (:node-genes genome)))
        outputs (mapv #(dec (:id %)) (filter #(= :output (:type %)) (:node-genes genome)))
        links   (vec (filter :enabled? (:connection-genes genome)))
        preact  (double-array (count (:node-genes genome)) 0.0)
        postact (double-array (count (:node-genes genome)) (into [1.0] inputs))]
    (loop [^long act-cyc activation-cycles
           err (double 99999999.0)]
      (if (or (= act-cyc 0) (< err threshold))
        (mapv #(aget postact %) outputs)
        (do
          (dorun (doseq [l links]
                   (aset preact (dec (:out l)) (+ (aget preact (dec (:out l)))
                                                  (* (:weight l)
                                                     (aget postact (dec (:in l))))))))
          (recur (dec ^long act-cyc)
                 (doubles-map-to-with-diff
                  @transfer-fun
                  preact
                  postact
                  :starting-index output-index)))))))


