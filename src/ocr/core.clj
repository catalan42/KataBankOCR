(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.set             :as set]
    [clojure.core.incubator  :as cci]
    [ocr.log                 :as log]
  ))

(log/set-min-level log/EXTRA)

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
    (map #(str/join (flatten [%1 %2])) 
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

(def ^:const digit-patterns [ " _     _  _     _  _  _  _  _ "
                              "| |  | _| _||_||_ |_   ||_||_|"
                              "|_|  ||_  _|  | _||_|  ||_| _|" ] )

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
          _ (assert (= 3 (first dims) )) ; number of lines
          _ (assert (= 0 (rem (second dims) 3) )) ; multiple of 3
        num-digits (/ (second dims) 3)
          _ (log/trace "num-digits" num-digits)

        dps2 (map #(partition 3 %) digits-str)
          _ (log/trace "shape dps2" (shape dps2))
          _ (assert (= (shape dps2) [3 num-digits 3] ))
          _ (do (log/trace (str \newline "dps2: " (shape dps2) ))
                (doseq [line dps2] 
                  (doseq [digit line]
                    (log/trace (str/join (flatten [\" digit "\" "] )))
                  )))
        dps3 (map vec (apply map concat dps2))
          _ (assert (= (shape dps3) [10 9] ))
          _ (do (log/trace (str \newline "dps3: " (shape dps3)))
                (doseq [digit dps3]
                  (log/trace ) 
                  (log/trace digit)
                  (log/trace (digit-to-str digit)) 
                ))
          _ (log/msg (str "All digits:\n" (digits-to-str dps3 )))
  ]

  )
)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []

  (parse-digits digit-patterns)

(comment
  (let [t1        (vec (map #(take 27 %) digit-patterns))
          _ (log/msg "t1:" )
          _ (doall (map #(log/msg (str/join %)) t1 ))
        t2 (parse-digits t1)
  ]
  )

  (log/dbg "digits-to-str")
  (log/dbg (digits-to-str [ "abcdefghi" "123456789" ] ))

  (assert (= (shape digit-patterns) [4 30] ))
  (def all-digits (parse-digits digit-patterns))
  (assert (= (count all-digits) 10 ))
)

)
(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded



(defn -main [& args]
  (log/dbg "Main program")
)

