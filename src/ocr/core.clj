(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.set             :as set]
    [clojure.core.incubator  :as cci]
    [ocr.log                 :as log]
  ))

(log/set-min-level log/EXTRA)

(def ^:const digit-patterns [ " _     _  _     _  _  _  _  _ "
                              "| |  | _| _||_||_ |_   ||_||_|"
                              "|_|  ||_  _|  | _||_|  ||_| _|" ] )

(defn shape
  "Return a vector of the dimensions of a nested seqs (e.g. a 4x3 array 
   would return [4 3]). Only the first element of each seq is evaluated 
   (i.e. a ragged array like [ [1 2] [1 2 3]] would report a shape of [2 2])."
   [array-seq]
   (if-not (cci/seqable? array-seq)
     []
     (let [curr-dim         (count array-seq)
           sub-shape        (shape (first array-seq)) ]
       (into [curr-dim] sub-shape)) ))

(defn digit-to-str
  "Format a digit into 3 line string."
  [digit]
  { :pre [ (= 9 (count digit)) ] }
  (str/join 
    (mapv #(str/join (flatten [%1 %2])) 
           (partition 3 digit)  (repeat \newline) ))
)

(defn digits-to-str
  "Format a sequence of digits into a 3 line string."
  [digits]
  { :pre [ (= 9 (second (shape digits))) ] }
  (apply str
    (for [out-row (range 3)]
      (str/join 
        (flatten [
          (for [digit digits] 
            (let [digit-rows (partition 3 digit) 
                  digit-line (nth digit-rows out-row) ]
              digit-line ))
          \newline ]  )))))

(defn parse-entry
  "Parse an account number entry from the machine."
  [entry]
  { :pre [ (= [4 27] (shape entry) )  ; 4 lines, 27 char/line
           (apply = (flatten [ \space (last entry) ] )) ; last line blank
         ] }
)

(defn parse-digits
  "Parse a string of digits from the machine."
  [digits-str]
  (let [dims  (shape digits-str)
          _ (assert (and (= 3 (first dims) ) ; number of lines
                         (= 0 (rem (second dims) 3)) )) ; multiple of 3
        num-digits (/ (second dims) 3)
          _ (log/trace "num-digits" num-digits)

        dps2 (mapv #(partition 3 %) digits-str)
          _ (log/trace "shape dps2" (shape dps2))
          _ (assert (= (shape dps2) [3 num-digits 3] ))
          _ (do (log/trace (str \newline "dps2: " (shape dps2) ))
                (doseq [line dps2] 
                  (doseq [digit line]
                    (log/trace (str/join (flatten [\" digit "\" "] )))
                  )))
        dps3 (apply mapv concat dps2)
          _ (do (log/trace (str \newline "dps3: " (shape dps3)))
                (doseq [digit dps3]
                  (log/trace ) 
                  (log/trace digit)
                  (log/trace (digit-to-str digit)) 
                ))
          _ (log/trace (str "All digits:\n" (digits-to-str dps3 )))
  ]
    dps3
  )
)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []

  (parse-digits digit-patterns)

  (let [  _ (assert (= (shape digit-patterns) [3 30] ))
        all-digits (parse-digits digit-patterns)
          _ (assert (= (shape all-digits ) [10 9] ))
          _ (log/msg (str "all-digits:" \newline (digits-to-str all-digits) ))

        t2 (parse-digits (mapv #(take 27 %) digit-patterns) )
          _ (log/msg (str "t2:" \newline (digits-to-str t2 )))
          _ (assert (= (shape t2) [9 9] ))

        t3 (parse-digits (mapv #(->>  %
                                      (drop  3 )
                                      (take 27 ) ) digit-patterns) )
          _ (log/msg (str "t3:" \newline (digits-to-str t3 )))

        digits-map (zipmap [:zero :one :two :three :four :five :six :seven :eight :nine ]
                           all-digits )

          _ (doseq [ dig-name (keys digits-map) ]
              (log/msg (str \newline dig-name \newline  
                            (digits-to-str [ (digits-map dig-name) ]) )))
  ]

  )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded



(defn -main [& args]
  (log/dbg "Main program")
)

