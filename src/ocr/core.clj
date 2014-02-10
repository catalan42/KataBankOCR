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

(def digit-to-lines 
  "Format a single digit into 3 separate lines"
  (memoize 
    (fn [digit]
      { :pre [ (= [9] (shape digit)) ] }
      (vec (partition 3 digit)) )))

(defn digit-to-str
  "Format a digit into a single 3-line string."
  [digit]
  { :pre [ (= 9 (count digit)) ] }
  (str/join (flatten 
    [ (interpose \newline  (digit-to-lines digit)) ] )))

(defn digits-to-str
  "Format a sequence of digits into a single 3-line string."
  [digits]
  { :pre [ (= 9 (second (shape digits))) ] }
  (apply str
    (for [out-row (range 3)]
      (str/join 
        (flatten [
          (for [digit digits] 
            (let [digit-rows (digit-to-lines digit)
                  digit-line (nth digit-rows out-row) ]
              digit-line ))
          \newline ]  )))))

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

(def all-digits  (parse-digits digit-patterns) )
(def digit-keys    [:zero :one :two :three :four :five :six :seven :eight :nine ] )
(def digits-map  (zipmap digit-keys all-digits ))

(defn parse-entry
  "Parse an account number entry from the machine."
  [entry]
  { :pre [ (= [4 27] (shape entry) )  ; 4 lines, 27 char/line
            ; last line blank
         ] 
    :post [ (= (shape %) [9 9] ) ] }
  (log/msg "flatten...'" (str/join (flatten [ \space (last entry) ] )) "'" )
  (apply = (flatten [ \space (last entry) ] ))
  (parse-digits (take 3 entry))
)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []

  (log/msg ":three")
  (log/msg (digit-to-str  (digits-map :three)))

  (parse-digits digit-patterns)

  (let [  _ (assert (= (shape digit-patterns) [3 30] ))
          _ (assert (= (shape all-digits ) [10 9] ))
          _ (log/dbg (str "all-digits:" \newline (digits-to-str all-digits) ))

        t2 (parse-digits (mapv #(take 27 %) digit-patterns) )
          _ (log/msg (str "t2:" \newline (digits-to-str t2 )))

        t3 (parse-digits (mapv #(->>  %
                                      (drop  3 )
                                      (take 27 ) ) digit-patterns) )
          _ (log/msg (str "t3:" \newline (digits-to-str t3 )))

        n123 (mapv digits-map [ :one :two :three ] )
          _ (log/msg (str "n123" \newline (digits-to-str n123)))

        n19 (mapv digits-map [ :one :two :three :four :five :six :seven :eight :nine ] )
          _ (log/msg "(shape n19)" (shape n19) )

        n19-str [ "    _  _     _  _  _  _  _ "
                  "  | _| _||_||_ |_   ||_||_|"
                  "  ||_  _|  | _||_|  ||_| _|" 
                  "                           " ]
          _ (log/msg "(shape n19-str) " (shape n19-str) )
          _ (log/msg (str "(last n19-str) '" (last n19-str) \' ))
        t4 (parse-entry n19-str)
          _ (log/msg "t4" (shape t4) )
        t5 (digits-to-str t4)
          _ (log/msg "t5" (shape t5) )
          _ (log/msg t5)
  ]

  )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded



(defn -main [& args]
  (log/dbg "Main program")
)

