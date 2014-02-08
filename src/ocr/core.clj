(ns ocr.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
    [ocr.log         :as log]
  ))

(log/set-min-level log/DEBUG)

(def digit-patterns " #  _     _  _     _  _  _  _  _  #
                   # | |  | _| _||_||_ |_   ||_||_| #
                   # |_|  ||_  _|  | _||_|  ||_| _| # " )

(defn demux-pat
  "Demultiplex the pattern for each digit 0..9 from the input pattern sequence."

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  ; Pull out the patterns, composed of space, pipe, and underscore
  (def pat-str "# [ |_]{30} #" )
  (def digits-pat-seq 
    (re-seq (re-pattern pat-str) digit-patterns) )
  (assert (= digits-pat-seq [ "#  _     _  _     _  _  _  _  _  #"
                              "# | |  | _| _||_||_ |_   ||_||_| #"
                              "# |_|  ||_  _|  | _||_|  ||_| _| #" ] ))
  (log/dbg)
  (log/dbg "digit-patterns:" )
  (doseq [ line digits-pat-seq ] (log/dbg line) )

)
(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded


(defn main [& args]

)

