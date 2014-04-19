(ns nef.graphviz-enabled
  (:require [rhizome
             [viz :as viz]
             [dot :as dot]])
  (:import [javax.swing JFrame ImageIcon]))

(def ^:dynamic *show-edge-label* false)
(def ^:dynamic *show-node-label* true)
(def ^:dynamic *graphviz-dpi* 96)

(defprotocol GraphvizEnabled
  (save-image [this filename] "Saves the image of this")
  (save-dot [this filename] "Saves the dot source for this")
  (view [this] [this frame] [this frame title] "Shows this in frame with title"))

(defn- -do-tree-grammar-function [fun grammar]
  (fun
   #(and (seq? %) (seq? (second %)))
   rest grammar
   :options {:dpi *graphviz-dpi*}
   :node->descriptor (fn [n]
                       (when-not (symbol? n)
                         {:label (.toString (first n))}))))

(extend-type clojure.lang.PersistentList
  GraphvizEnabled
  (save-image [this filename]
      (viz/save-image (-do-tree-grammar-function
                       viz/tree->image
                       this)
                      filename))
    (save-dot [this filename]
      (spit filename
            (-do-tree-grammar-function dot/tree->dot this)))
    (view
      ([this]
         (view this (viz/create-frame "Symbolic Expression Tree")))
      ([this frame]
         (let [[^JFrame frame ^ImageIcon image-icon] @frame]
           (.setImage image-icon (-do-tree-grammar-function viz/tree->image this))
           (if (.isShowing frame)
             (.repaint frame)
             (.setVisible frame true))))
      ([this frame title]
         (let [[^JFrame frame ^ImageIcon image-icon] @frame]
           (.setTitle frame title)
           (.setImage image-icon (-do-tree-grammar-function viz/tree->image this))
                      (if (.isShowing frame)
             (.repaint frame)
             (.setVisible frame true))))))

