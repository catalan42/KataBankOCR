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
(def ^:const digit-keys [ :zero :one :two :three :four 
                          :five :six :seven :eight :nine ] )

(defn shape
  "Return a vector of the dimensions of a nested seqs (e.g. a 4x3 array 
   would return [4 3]). Only the first element of each seq is evaluated 
   (i.e. a ragged array like [ [1 2] [1 2 3]] would return a shape of [2 2])."
   [array-seq]
   (if-not (cci/seqable? array-seq)
     []  ; (shape <scalar>) is []
     (let [curr-dim         (count array-seq)
           sub-shape        (shape (first array-seq)) ]
       (into [curr-dim] sub-shape)) ))

(defn digpats-to-lines
  "Format a sequence of digit patterns into 3 separate lines"
  [digits] ; shape=[n  9]
  { :pre [ (= 9 (second (shape digits))) ] }
  (let [ tmp-n-3-3  (mapv #(partition 3 %) digits)  ; shape=[n 3 3]
         tmp-3n-3   (apply mapv concat tmp-n-3-3)   ; shape=[3n 3]
  ] tmp-3n-3 ))

(defn lines-to-str
  "Format a sequence of 3 lines into a single 3-line string (including newlines)."
  [lines]
  { :pre [ (= 3 (count lines)) ] }
  (str/join (flatten [ (interpose \newline  lines) ] )))

(defn digpats-to-str
  "Format a sequence of digit patterns into a single 3-line string."
  [digits]
  { :pre [ (= 9 (second (shape digits))) ] }
  (->> digits
      (digpats-to-lines )
      (lines-to-str   ) ))

(defn digpat-to-str
  "Format a single digit pattern into a single 3-line string."
  [digit]
  { :pre [ (= 9 (count digit)) ] }
  (digpats-to-str [digit]) )

(defn parse-digits
  "Parse a string of digits from the machine."
  [digits-str]
  (let [dims  (shape digits-str)
          _ (assert (and (= 3 (first dims) )            ; 3 lines
                         (= 0 (rem (second dims) 3)) )) ; multiple of 3
        num-digits (/ (second dims) 3)
          _ (log/trace "num-digits" num-digits)

        dps2 (mapv #(partition 3 %) digits-str)
          _ (log/msg "shape dps2" (shape dps2))
          _ (assert (= (shape dps2) [3 num-digits 3] ))
        dps3 (apply mapv concat dps2)
          _ (do (log/msg (str \newline "dps3: " (shape dps3)))
                (doseq [digit dps3]
                  (log/msg ) 
                  (log/msg digit)
                  (log/msg (digpat-to-str digit)) 
                ))
          _ (log/msg (str "All digits:\n" (digpats-to-str dps3 )))
  ]
    dps3
  )
)

(def all-digits  (parse-digits digit-patterns) )
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
  (log/msg (digpat-to-str  (digits-map :three)))

  (parse-digits digit-patterns)

  (let [  _ (assert (= (shape digit-patterns) [3 30] ))
          _ (assert (= (shape all-digits ) [10 9] ))
          _ (log/dbg (str "all-digits:" \newline (digpats-to-str all-digits) ))

        t2 (parse-digits (mapv #(take 27 %) digit-patterns) )
          _ (log/msg (str "t2:" \newline (digpats-to-str t2 )))

        t3 (parse-digits (mapv #(->>  %
                                      (drop  3 )
                                      (take 27 ) ) digit-patterns) )
          _ (log/msg (str "t3:" \newline (digpats-to-str t3 )))

        n123 (mapv digits-map [ :one :two :three ] )
          _ (log/msg (str "n123" \newline (digpats-to-str n123)))

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
        t5 (digpats-to-str t4)
          _ (log/msg "t5" (shape t5) )
          _ (log/msg t5)
  ]

  )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded



(defn -main [& args]
  (log/dbg "Main program")
)

