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

(def ^:const max-fix-distance 
  "Upper limit on the Mahalanobis distance allowed between the original and corrected
  versions of an entry."
  1 )

(defn truthy?
  "Returns true if argument is logical true (neither nil nor false);
  otherwise returns false."
  [arg]
  (if arg true false) )

(defn any?
  "Returns true if (pred x) is logical true for any x in coll, else false.
  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn conjv 
  "Appends to a collection, always returning a vector."
  [coll item]
  (conj (vec coll) item) )

(defn shape
  "Return a vector of the dimensions of a nested seqs (e.g. a 4x3 array 
   would return [4 3]). Only the first element of each seq is evaluated 
   (i.e. a ragged array like [ [1 2] [1 2 3]] would return a shape of [2 2])."
   [item]
   (if-not (cci/seqable? item)
     []  ; (shape <scalar>) => []
     (let [curr-dim   (count item)
           sub-shape  (shape (first item)) ]
       (into [curr-dim] sub-shape) )))

(defn lines->str
  "Format a sequence of lines into a single multi-line string (including newlines)."
  [lines]
  (str/join (flatten [ (interpose \newline  lines) ] )))

(defn parse-digits
  "Parse a set of 3 scan lines from the machine. Returns a digit pattern."
  [scan-lines]
  { :pre [(let [ [nrows ncols] (shape scan-lines) ]
            (and (= 3 nrows )             ; 3 lines
                 (= 0 (rem ncols 3)) )) ] ; multiple of 3
  }
  (->> scan-lines                   ; shape=[3 3n]   where n=num-digits
       (mapv #(partition 3 %) )     ; shape=[3 n 3]
       (apply mapv concat     ) ))  ; shape=[n 9]

(defn parse-entry
  "Parse an account number entry from the machine. Returns a vector of digit patterns"
  [entry]
  { :pre [ (= [4 27] (shape entry) )                 ; 4 lines, 27 char/line
           (apply = (into [\space] (last entry))) ]  ; last line blank
    :post [ (= (shape %) [9 9] ) ] }
  (vec (parse-digits (take 3 entry))) ) ; parse only first 3 scan lines of entry

(def all-digpats     (parse-digits all-digit-lines))     ; shape=[10 9]
(def digkey->digpat  (zipmap all-digkeys all-digpats ))  ; maps digit-key -> digit-pattern
(def digpat->digkey  (zipmap all-digpats all-digkeys ))  ; maps digit-pattern -> digit-key

; Maps from digit-key to numeric digit. Invalid digits found during parsing will result in
; a digit-key value of nil.  Map nil digkey values to the '?' character for output.
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
  "Converts a sequence of digit-pattern values into a vector of digit-key values. Output
  vector will contain nil for unrecognized (i.e. illegible) digit-patterns."
  [digpats]
  (mapv digpat->digkey digpats) )

(defn digkeys->digpats
  "Converts a sequence of digit-key values into a vector of digit-pattern values"
  [digkeys]
  (mapv digkey->digpat digkeys) )

(defn digkeys->lines
  "Converts a sequence of digit-key values into a 3-line digit pattern."
  [digkeys]
    (->> digkeys
        (digkeys->digpats )
        (digpats->lines   ) ))

(defn digkeys->digstr
  "Converts a sequence of digit-key values into a string of digit characters.
  Invalid (i.e. nil) digit-key values will become '?' characters in output string."
  [digkeys]
  (str/join (mapv digkey->digit digkeys)) )

(defn valid-digkeys?
  "Returns true if all digit-keys are legible (i.e. not nil), else false."
  [digkeys]
  (every? truthy? digkeys) )

(def ^:const checksum-coeffs (->> (range 1 10) reverse vec )) ; [9..1]

(defn entry-valid?
  "Returns true if all digit-keys for an entry are legible, and a valid checksum is
  computed; otherwise, returns false.  "
  [digkeys]
  (truthy?
    (when (valid-digkeys? digkeys)
      (->> digkeys
           (mapv digkey->digit     )
           (mapv * checksum-coeffs )
           (reduce +    )
           (#(mod % 11) )
           (= 0 ) ))))

(defn digpats-valid?
  "Returns true if all digit-patterns for an entry are legible, and a valid checksum is
  computed; otherwise, returns false.  "
  [digpats]
  (->> digpats 
       digpats->digkeys  
       entry-valid? ))

(defn calc-pattern-dist
  "Returns the Mahalanobis distance between a entry digit-pattern and each the canonical digit
  pattern, calculated as the number of elements where they differ."
  [entry-digpats]
  (vec (for [entry-pat entry-digpats]
    (vec (for [canon-pat all-digpats]
      (let [deltas      (map #(not= %1 %2) entry-pat canon-pat)
            num-deltas  (count (filter truthy? deltas)) ]
        num-deltas ))))))

(defn calc-fix-digstrs
  "Returns all possible 'fixed' digit-strings for the current entry. These are
  digit-strings that have valid checksums and are within the maximum allowed distance of
  the entry pattern."
  [entry-digpats]
  (let [fix-dists     (calc-pattern-dist entry-digpats)
        swap-list     (filter truthy? (flatten
                        (for [iDigit (range (count entry-digpats)) ]  ; entry digpat idx
                          (for [iCanon (range (count all-digpats)) ]  ; canonical digpat idx
                            (let [dist (get-in fix-dists [iDigit iCanon]) ]
                              (when (<= dist max-fix-distance) 
                                {:iDigit iDigit :iCanon iCanon :dist dist}
                              ) )))))
        poss-digkeys  (filter truthy? (flatten
                        (for [swapper swap-list]
                          (let [mod-digpats 
                                    (assoc entry-digpats 
                                        (:iDigit swapper) ; idx of digpat to replace
                                        (all-digpats (:iCanon swapper)) ) ; digpat to use
                                mod-digkeys (->> mod-digpats digpats->digkeys)
                          ] (when (entry-valid? mod-digkeys)
                              {:val mod-digkeys} )
                          ))))
        fix-digstrs  (mapv #(digkeys->digstr (:val %) ) poss-digkeys)
  ] fix-digstrs ))

(defn display-msg
  "Displays a nicely format status message."
  ; Sample output:
  ;   =>   000000000                                  - valid entry read
  ;   =>   711111111 FIX                              - invalid entry, but fixed uniquely
  ;   =>   222222222 ERR                              - invalid entryd, no legal fix
  ;   =>   555555555 AMB ['559555555', '555655555']   - invalid entryd, multiple legal fixes
  ;   =>   1234?678? ILL                              - illegible entry, no legal fix
  [digit-str status-str & ambiguous-str]
  (log/msg "=>  " digit-str status-str (apply str ambiguous-str)) )

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
      (reduce conjv []
        (map #(hash-map :entry %1  :expected %2)
                         entries    expected-strs )))))

(defn run []
  (doseq [test-case test-data]
    (let [digpats           (parse-entry (:entry test-case))
          digkeys           (->> digpats digpats->digkeys)
          entry-valid       (entry-valid? digkeys)
          entry-illegible   (not (valid-digkeys? digkeys))
          entry-digstr      (->> digkeys digkeys->digstr) 
          status-str        (if entry-illegible "ILL" 
                              (if-not entry-valid "ERR" "   " ) )
    ]
      (log/msg)
      (log/msg (digpats->str digpats))
      (log/ext "exp:" (:expected test-case) )
      (if entry-valid
        (display-msg entry-digstr status-str )
      ;else - invalid entry read from machine
        (let [ fix-digstrs (calc-fix-digstrs digpats)
        ] (cond (= 1 (count fix-digstrs))
                  ; Only 1 possible correct value with (dist=1). Report it as the answer but
                  ; label it as "FIX" to indicate auto-correct has occurred.
                  (let [fixed-digstr (first fix-digstrs) ]
                    (display-msg fixed-digstr "FIX") )
                (< 1 (count fix-digstrs))
                  ; Multiple possible correct values with (dist=1) exist. Report the
                  ; original scanned string and the possible ambiguous account numbers.
                  (let [amb-digstr (->>  fix-digstrs
                                         (mapv #(format "'%s'" %) )
                                         (interpose ", "  )
                                         (apply str       )
                                         (format "[%s]"   )  
                                   )]
                    (display-msg entry-digstr "AMB" amb-digstr ) )
                :else ; no corrections found
                    ; Scanned value is incorrect but all values with (dist=1) are invalid.
                    (display-msg entry-digstr status-str)
          ) )))))

(defn do-tests []
  (log/dbg "********************************************************************************")
  (log/dbg "Running tests...")

  (assert      (truthy?  true   ))
  (assert      (truthy?  :a     ))
  (assert      (truthy?  0      ))  ; zero is not falsey!
  (assert (not (truthy?  false )))
  (assert (not (truthy?  nil   )))

  (let [x23 [ [1 2 3]
              [4 5 6] ]
        x32 (apply mapv vector x23)  ; map is like a matrix transpose
  ] 
    (log/dbg "x23" x23 )
    (log/dbg "x32" x32 )
    (assert (= (shape x23) [2 3])) 
    (assert (= (shape x32) [3 2])) 
  )
  (assert (= (shape all-digit-lines) [3 30] )) ; canonical lines
  (assert (= (shape all-digpats)     [10 9] )) ; canonical digit-patterns
  (assert (= (str/join (digkey->digpat :nine) ) " _ |_| _|" )) ; sample digit-pattern

  (assert (=  (digpats->lines 
                (parse-digits 
                  (mapv #(take 18 %) all-digit-lines) ))
              [ " _     _  _     _ "
                "| |  | _| _||_||_ "
                "|_|  ||_  _|  | _|" ] ))

  (log/dbg "digit patterns 2-5:" )
  (log/dbg  (digpats->str 
              (parse-digits 
                (mapv #(->> % (drop  6 ) (take 12 ) ) all-digit-lines) )))

  (log/dbg "digkeys 123" )
  (log/dbg  (lines->str 
              (digkeys->lines [ :one :two :three ]) ))

  (assert (= (->> all-digpats digpats->digkeys) all-digkeys ))
  (assert (= (->> all-digkeys digkeys->digpats) all-digpats ))

  (assert (= (digpats->lines all-digpats)
              [" _     _  _     _  _  _  _  _ "
               "| |  | _| _||_||_ |_   ||_||_|"
               "|_|  ||_  _|  | _||_|  ||_| _|"] ))

  (assert (= (->> all-digkeys digkeys->lines) all-digit-lines ))

  (let [entry-1-9 ["    _  _     _  _  _  _  _ "
                   "  | _| _||_||_ |_   ||_||_|"
                   "  ||_  _|  | _||_|  ||_| _|"
                   "                           " ]
        ent19-digpats (parse-entry entry-1-9) ]
    (assert (=  (->> ent19-digpats digpats->digkeys)
                [ :one :two :three :four :five :six :seven :eight :nine ] ))
    (log/dbg)
    (log/dbg "ent19-digpats")
    (log/dbg (digpats->str ent19-digpats)) 
  )

  (log/dbg)
  (log/dbg "all-digpats conversion")
  (log/dbg (->> all-digpats
                (digpats->digkeys )
                (digkeys->lines   )
                (lines->str       ) ))

  (log/dbg "digkey->digpat" )
  (doseq [digkey all-digkeys]
    (log/dbg (format "%-10s" digkey) (digkey->digpat digkey)) )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code is loaded

(defn -main [& args]
  (run) )

