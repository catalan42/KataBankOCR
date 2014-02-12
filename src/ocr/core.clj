(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.core.incubator  :as cci]
    [ocr.log                 :as log]
  ))

(log/set-min-level log/MESSAGE)

(def ^:const all-digit-lines 
  "Master pattern defintion for machine output digit patterns (in order)"
  [ " _     _  _     _  _  _  _  _ "
    "| |  | _| _||_||_ |_   ||_||_|"
    "|_|  ||_  _|  | _||_|  ||_| _|"] )

(def ^:const all-digkeys 
  "Master list of digit keywords (in order)"
  [ :zero :one :two :three :four :five :six :seven :eight :nine ] )

(def ^:const all-digits
  "Master list of digits (in order)"
  (vec (range 10)) )

(def ^:const checksum-coeffs (vec (->> (range 10) (drop 1) (reverse)) )) ; [9..1]

; awtawt todo:  write shape-strict fn
(defn shape
  "Return a vector of the dimensions of a nested seqs (e.g. a 4x3 array 
   would return [4 3]). Only the first element of each seq is evaluated 
   (i.e. a ragged array like [ [1 2] [1 2 3]] would return a shape of [2 2])."
   [array-seq]
   (if-not (cci/seqable? array-seq)
     []  ; (shape <scalar>) => []
     (let [curr-dim   (count array-seq)
           sub-shape  (shape (first array-seq)) ]
       (into [curr-dim] sub-shape) )))

(defn lines-to-str
  "Format a sequence of 3 lines into a single 3-line string (including newlines)."
  [lines]
  { :pre [ (= 3 (count lines)) ] }
  (str/join (flatten [ (interpose \newline  lines) ] )))

(defn parse-digits
  "Parse a set of 3 digit lines from the machine. Returns a digit pattern."
  [digit-lines]
  { :pre [(let [ [nrows ncols] (shape digit-lines) ]
             (and (= 3 nrows )             ; 3 lines
                  (= 0 (rem ncols 3)) )) ] ; multiple of 3
  }
  (->> digit-lines                  ; shape=[3 3n]   where n=num-digits
       (mapv #(partition 3 %) )     ; shape=[3 n 3]
       (apply mapv concat     ) ))  ; shape=[n 9]

(defn parse-entry
  "Parse an account number entry from the machine. Returns a vector of digit patterns"
  [entry]
  { :pre [ (= [4 27] (shape entry) )  ; 4 lines, 27 char/line
           (apply = (into [\space] (last entry))) ] ; last line blank
    :post [ (= (shape %) [9 9] ) ] }
  (vec (parse-digits (take 3 entry))) )

(def all-digpats    (parse-digits all-digit-lines))
(def digkey-digpat  (zipmap all-digkeys all-digpats ))
(def digpat-digkey  (zipmap all-digpats all-digkeys ))
(def digkey-digit   (zipmap all-digkeys all-digits  ))

(defn digpats-to-lines
  "Format a sequence of digit patterns into 3 separate lines"
  [digpats] ; shape=[n 9]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats                       ; shape=[n 9]
       (mapv #(partition 3 %) )      ; shape=[n 3 3]
       (apply mapv concat     )      ; shape=[3n 3]
       (mapv str/join         )))    ; convert to string

(defn digpats-to-str
  "Format a sequence of digit patterns into a single 3-line string."
  [digpats]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats
      (digpats-to-lines )
      (lines-to-str     ) ))

(defn digpats-to-digkeys
  "Covert a sequence of digit-pattern values into a vector of digit-key values"
  [digpats]
  (mapv digpat-digkey digpats) )

(defn digkeys-to-digpats
  "Covert a sequence of digit-key values into a vector of digit-pattern values"
  [digkeys]
  (mapv digkey-digpat digkeys) )

(defn digkeys-to-lines
  "Covert a sequence of digit-key values into a 3-line digit pattern."
  [digkeys]
  (digpats-to-lines 
    (digkeys-to-digpats digkeys)) )

(defn digkeys-to-digitstr
  "Covert a sequence of digit-key values into a string of digit characters"
  [digkeys]
  (str/join (mapv digkey-digit digkeys)) )

(def test-data   
  "A collection of all test data-lines and expected results"
  (let [data-lines      (vec (str/split-lines 
                               (slurp "resources/test-data.txt")) )
        num-data-lines  (/ (count data-lines)  4) ; 4 lines per entry
        expected-strs   (vec (->> (slurp "resources/test-result.txt")
                                  (str/split-lines )
                                  (map str/trim    ) )) ]
    (assert (=  num-data-lines (int num-data-lines) )) ; no partial entries
    (assert (=  num-data-lines (count expected-strs))) ; equal numbers

    (let [entries (partition 4 data-lines)]
      (reduce conj []
        (map #(hash-map :entry %1  :expected %2)
                         entries    expected-strs )))))
(comment

  (mapv (fn [value] { :value value } ) [:a :b :c])
  (mapv #(          { :value %1    } ) [:a :b :c])
  (mapv #(hash-map    :value %1      ) [:a :b :c])

)

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  (log/msg "********************************************************************************")
  (log/msg "Running tests...")

  (assert (= (shape all-digit-lines) [3 30] ))
  (assert (= (shape all-digpats)     [10 9] ))
  (assert (= (str/join (digkey-digpat :nine) ) " _ |_| _|" ))

  (assert (=  (digpats-to-lines 
                (parse-digits 
                  (mapv #(take 18 %) all-digit-lines) ))
              [ " _     _  _     _ "
                "| |  | _| _||_||_ "
                "|_|  ||_  _|  | _|" ] ))

  (log/trace "digit patterns 2-5:" )
  (log/trace  (digpats-to-str 
                (parse-digits 
                  (mapv #(->> % (drop  6 ) (take 12 ) ) all-digit-lines) )))

  (log/trace "digkeys 123" )
  (log/trace  (lines-to-str 
                (digkeys-to-lines [ :one :two :three ]) ))

  (assert (= (digpats-to-digkeys all-digpats) all-digkeys ))
  (assert (= (digkeys-to-digpats all-digkeys) all-digpats ))

  (assert (= (digpats-to-lines all-digpats)
              [ " _     _  _     _  _  _  _  _ "
                "| |  | _| _||_||_ |_   ||_||_|"
                "|_|  ||_  _|  | _||_|  ||_| _|"] ))

  (assert (= (digkeys-to-lines all-digkeys) all-digit-lines ))

  (let [entry-1-9 [ "    _  _     _  _  _  _  _ "
                    "  | _| _||_||_ |_   ||_||_|"
                    "  ||_  _|  | _||_|  ||_| _|"
                    "                           " ]
        ent19-digpats (parse-entry entry-1-9) ]
    (assert (=  (digpats-to-digkeys ent19-digpats)
                [ :one :two :three :four :five :six :seven :eight :nine ] ))
    (log/trace)
    (log/trace "ent19-digpats")
    (log/trace (digpats-to-str ent19-digpats)) 
  )

  (log/trace)
  (log/trace "all-digpats conversion")
  (log/trace (->> all-digpats
                  (digpats-to-digkeys )
                  (digkeys-to-lines   )
                  (lines-to-str       ) ))
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded

; checksum calculation:
; (d1+2*d2+3*d3 +..+9*d9) mod 11 = 0

(defn calc-checksum
  "Calculate the checksum from a sequence of digkeys."
  [digkeys]
  (mapv digkey-digit digkeys)
)

(defn -main [& args]
  (log/msg "Main program")
  (log/msg "(count test-data)" (count test-data) )
  (doseq [sample test-data ]
    (let [digpats     (parse-entry (:entry sample))
          digit-str   (->> digpats digpats-to-digkeys digkeys-to-digitstr) ]
      (log/msg "\n\n")
      (log/msg "digpats:")
      (log/msg (digpats-to-str digpats))

      (log/msg "digit-str " digit-str )
      (log/msg "expected  " (:expected sample) )
    ) ))
