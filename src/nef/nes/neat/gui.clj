(ns nef.nes.neat.gui
  (:require [seesaw 
             [core :as sc]]
            [nef.nes.neat 
             [evolution-parameters :as ep]]))


(defn- create-widget-for
  [id type value default-value]
  (case type
    :float (sc/spinner :id id :model (sc/spinner-model (double default-value)
                                                       :from (and (seq value) (double (first value)))
                                                       :to (and (second value) (double (second value)))
                                                       :by 0.001))
    :int   (sc/spinner :id id :model (sc/spinner-model (int default-value)
                                                       :from (and (seq value) (int (first value)))
                                                       :to (and (second value) (int (second value)))
                                                       :by 1))
    :range (sc/text :id id :text (str (first default-value)
                                      ";"
                                      (second default-value)))
    :any-of (let [x (sc/listbox :id id :model value
                                :selection-mode :multi-interval)]
              (sc/selection! x {:multi? true} default-value)
              x)
    :boolean (sc/checkbox :id id :selected? default-value)
    (sc/label :id id :text "Cannot be set yet!")))


(defn- create-option-widget
  [option]
  [(sc/label :text (:name option)
             :tip  (:description option))
   (create-widget-for (:id option)
                      (:type option) 
                      (:value option)
                      @(:var option))])

(defn- create-id-selector
  [s]
  (keyword (clojure.string/replace-first (str s) \: \#)))

(defn- vec* 
  [coll]
  (if (seq coll)
    (vec coll)
    (if coll 
      [coll]
      nil)))

(defn- get-new-settings
  [panel]
  (into {} (mapv (fn [[id type]] 
                   [id (let [w  (sc/select panel [(create-id-selector id)])]
                         (case type 
                           :range (mapv (comp double read-string) (clojure.string/split (sc/value w) #"\s*;\s*"))
                           :any-of (vec* (sc/selection w {:multi? true}))
                           (sc/value w)))])
                 (mapv (juxt :id :type) @ep/options))))

(defn set-new-settings
  [new-settings]
  (dosync
   (dorun (doseq [opt @ep/options]
            (if (and (#{:float :int :range :any-of :boolean} (:type opt))
                     (new-settings (:id opt)))
              (ref-set (:var opt) (new-settings (:id opt))))))))

(defn get-settings []
  (into {} (mapv (fn [opt]
                   (if (#{:float :int :range :any-of :boolean} (:type opt))
                     (vector (:id opt) @(:var opt))))  @ep/options)))

(defn print-settings
  []
  (prn (get-settings)))

(defn- create-options
  []
  (let [widgets (vec (mapcat create-option-widget @ep/options))
        panel (sc/grid-panel :columns 2 :items widgets)]
    panel))

(defn show-options
  []
  (let [panel (create-options)]
    (-> (sc/dialog :content (sc/scrollable panel) :title "Settings"
                   :option-type :ok-cancel
                   :success-fn  (fn [e] 
                                  (set-new-settings (get-new-settings panel))))
        sc/pack!
        sc/show!)))



