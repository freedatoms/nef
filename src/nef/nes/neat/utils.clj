(ns nef.nes.neat.utils
  (:require [taoensso.nippy :as nippy]))

(defn freeze-to-file [file obj]
  (with-open [o (java.io.BufferedOutputStream.
                 (java.io.FileOutputStream. file))]
    (.write o (nippy/freeze obj))))

(defn thaw-from-file [file]
  (let [fil (java.io.File. file)]
    (with-open [i (java.io.FileInputStream. fil)]
      (let [^bytes b (byte-array (.length fil))]
        (.read i b)
        (nippy/thaw b)))))
