(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.set             :as set]
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

(defn truthy?
  "Returns true if argument is logical true (neither nil nor false);
  othersise returns false."
  [arg]
  (if arg true false) )

(defn any?
  "Returns true if any (pred x) in coll is logical true, else false.
  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (if (some pred coll) true false) )

; Generic vectors (tuples) for use as ad-hoc containers. Since these are distinct types
; different from generic Clojure vectors or maps, they are easier to use with conj,
; flatten, etc.
(defrecord Pair   [a b    ]) ; generic 2-vec
(defrecord Triple [a b c  ]) ; generic 3-vec
(defrecord Quad   [a b c d]) ; generic 4-vec

(defn conjv 
  "Appends to a collection, always returning a vector."
  [coll item]
  (conj (vec coll) item) )

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

(defn lines->str
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

(def all-digpats     (parse-digits all-digit-lines))
(def digkey->digpat  (zipmap all-digkeys all-digpats ))
(def digpat->digkey  (zipmap all-digpats all-digkeys ))
(def digkey->digit   (conj (zipmap all-digkeys all-digits ) { nil \? } ))

(defn digpats->lines
  "Format a sequence of digit patterns into 3 separate lines"
  [digpats] ; shape=[n 9]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats                       ; shape=[n 9]
       (mapv #(partition 3 %) )      ; shape=[n 3 3]
       (apply mapv concat     )      ; shape=[3n 3]
       (mapv str/join         )))    ; convert to string

(defn digpats->str
  "Format a sequence of digit patterns into a single 3-line string."
  [digpats]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats
      (digpats->lines )
      (lines->str     ) ))

(defn digpats->digkeys
  "Covert a sequence of digit-pattern values into a vector of digit-key values. Output
  vector will contain nil for unrecognized (i.e. illegible) digit-patterns."
  [digpats]
  (mapv digpat->digkey digpats) )

(defn digkeys->digpats
  "Covert a sequence of digit-key values into a vector of digit-pattern values"
  [digkeys]
  (mapv digkey->digpat digkeys) )

(defn digkeys->lines
  "Covert a sequence of digit-key values into a 3-line digit pattern."
  [digkeys]
  (digpats->lines 
    (digkeys->digpats digkeys)) )

(defn digkeys->digitstr
  "Covert a sequence of digit-key values into a string of digit characters"
  [digkeys]
  (str/join (mapv digkey->digit digkeys)) )

(defn valid-digkeys?
  "Returns true if all digit-keys are legible (i.e. not nil), else false."
  [digkeys]
  (every? truthy? digkeys) )

(def ^:const checksum-coeffs (vec (->> (range 10) (drop 1) (reverse)) )) ; [9..1]
(defn entry-valid?
  "Returns true if all digit-keys for an entry are legible, and a valid checksum is
  computed; otherwise, returns false.  "
  [digkeys]
  (if (valid-digkeys? digkeys)
    (->> digkeys
         (mapv digkey->digit )
         (mapv * checksum-coeffs )
         (reduce + ) 
         (#(mod % 11) )
         (= 0 ) )
    false ))

(defn digpats-valid?
  "Returns true if all digit-patterns for an entry are legible, and a valid checksum is
  computed; otherwise, returns false.  "
  [digpats]
  (->> digpats 
       digpats->digkeys  
       entry-valid? ))

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

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  (log/msg "********************************************************************************")
  (log/msg "Running tests...")

  (assert      (truthy?  true   ))
  (assert      (truthy?  :a     ))
  (assert      (truthy?  "abc"  ))
  (assert      (truthy?  1      ))
  (assert      (truthy?  0      ))  ; zero is not falsey!
  (assert (not (truthy?  false )))
  (assert (not (truthy?  nil   )))

  (assert      (every? truthy?   [0 1 :a [1 2] "hello" true]  ))
  (assert (not (every? truthy?   [0 1 :a nil   "hello" true] )))
  (assert (not (every? truthy?   [0 1 :a false "hello" true] )))

  (assert (= (shape all-digit-lines) [3 30] ))
  (assert (= (shape all-digpats)     [10 9] ))
  (assert (= (str/join (digkey->digpat :nine) ) " _ |_| _|" ))

  (assert (=  (digpats->lines 
                (parse-digits 
                  (mapv #(take 18 %) all-digit-lines) ))
              [ " _     _  _     _ "
                "| |  | _| _||_||_ "
                "|_|  ||_  _|  | _|" ] ))

  (log/trace "digit patterns 2-5:" )
  (log/trace  (digpats->str 
                (parse-digits 
                  (mapv #(->> % (drop  6 ) (take 12 ) ) all-digit-lines) )))

  (log/trace "digkeys 123" )
  (log/trace  (lines->str 
                (digkeys->lines [ :one :two :three ]) ))

  (assert (= (digpats->digkeys all-digpats) all-digkeys ))
  (assert (= (digkeys->digpats all-digkeys) all-digpats ))

  (assert (= (digpats->lines all-digpats)
              [ " _     _  _     _  _  _  _  _ "
                "| |  | _| _||_||_ |_   ||_||_|"
                "|_|  ||_  _|  | _||_|  ||_| _|"] ))

  (assert (= (digkeys->lines all-digkeys) all-digit-lines ))

  (let [entry-1-9 [ "    _  _     _  _  _  _  _ "
                    "  | _| _||_||_ |_   ||_||_|"
                    "  ||_  _|  | _||_|  ||_| _|"
                    "                           " ]
        ent19-digpats (parse-entry entry-1-9) ]
    (assert (=  (digpats->digkeys ent19-digpats)
                [ :one :two :three :four :five :six :seven :eight :nine ] ))
    (log/trace)
    (log/trace "ent19-digpats")
    (log/trace (digpats->str ent19-digpats)) 
  )

  (log/trace)
  (log/trace "all-digpats conversion")
  (log/trace (->> all-digpats
                  (digpats->digkeys )
                  (digkeys->lines   )
                  (lines->str       ) ))

  (log/msg "digkey->digpat" )
  (doseq [digkey all-digkeys]
    (log/msg (format "%-10s" digkey) (digkey->digpat digkey)) )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code is loaded

(defn calc-pattern-dist
  "Returns the 'distance' between a sample digit-pattern and each the canonical digit
  pattern, calculated as the number of elements where they differ."
  [sample-digpats]
  (vec (for [sample-pat sample-digpats]
    (vec (for [canon-pat all-digpats]
      (let [deltas      (map #(not= %1 %2) sample-pat canon-pat)
            num-deltas  (count (filter truthy? deltas)) ]
        num-deltas ))))))

(defn -main [& args]
  (if true (do

  (log/msg "Main program")
  (log/msg "(count test-data)" (count test-data) )
  (doseq [sample test-data ]
    (let [sample-digpats    (parse-entry (:entry sample))
          digkeys           (->> sample-digpats digpats->digkeys)
          ent-invalid       (not (entry-valid? digkeys))
          illegible         (not (valid-digkeys? digkeys))
          num-ill           (count (filter nil? digkeys))
          digit-str         (->> digkeys digkeys->digitstr) 
          status-str        (if illegible "ILL" 
                              (if ent-invalid "ERR" "   " ) )
    ]
      (log/msg)
      (log/msg "sample-digpats:")
      (log/msg (digpats->str sample-digpats))
      (log/msg "digkeys:   " digkeys)
      (log/msg "expected:  " (:expected sample) )
      (log/msg "digit-str: " digit-str status-str  (str " (illegible:" illegible 
        "  num-ill:" num-ill "  ent-invalid:" ent-invalid ")" ) )
      (let [sample-dist (calc-pattern-dist sample-digpats) 
            swap-list   (filter truthy?
                          (flatten
                            (for [iSamp (range (count sample-digpats)) ]
                              (for [iCanon (range (count all-digpats)) ]
                                (let [dist (get-in sample-dist [iSamp iCanon]) ]
                                  (when (= 1 dist) 
                                    (Triple. iSamp iCanon dist )
                                  ) )))))
      ]
        (log/dbg "sample-dist, shape"    (shape sample-dist))
        (log/dbg sample-dist)
        (log/msg "swap-list" swap-list )
      ) 

    ) ))))
