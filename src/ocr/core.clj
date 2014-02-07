(ns ocr.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
    [ocr.log         :as log]
  ))

(log/set-min-level log/NORMAL)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  (assert true)
)
(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded


(defn main
  "I don't do a whole lot."
  [& args]
  (log/msg "Hello, World!"))
