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
  [pat1 pat2 pat3]
)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  ; Pull out the patterns, composed of space, pipe, and underscore
  (def pat-str "# [ |_]{30} #" )
  (def digits-pat-seq 
    (vec (re-seq (re-pattern pat-str) digit-patterns)) )
  (assert (= digits-pat-seq [ "#  _     _  _     _  _  _  _  _  #"
                              "# | |  | _| _||_||_ |_   ||_||_| #"
                              "# |_|  ||_  _|  | _||_|  ||_| _| #" ] ))
  (log/dbg)
  (log/dbg "digit-patterns:" )
  ; digits-pat-seq 
  (def dps1 (map #(->> %
                       (drop 2   )
                       (take 30  )
                       (str/join ) ) digits-pat-seq ))
  (doseq [ line dps1 ] (log/msg line) )

  (def dps2 (map (partial take 30) dps1) )
  (doseq [ line dps2 ] (log/msg line) )

  (def dps3 (map #(partition 3 %) dps2 ))
  (doseq [line dps3] 
    (doseq [digit line]
      (log/msg (str/join (flatten [\" digit "\" "] )))
    ))

)
(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded


(defn main [& args]

)

