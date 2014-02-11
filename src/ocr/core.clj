(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.core.incubator  :as cci]
    [ocr.log                 :as log]
  ))

(log/set-min-level log/EXTRA)

(def ^:const all-digit-lines 
  "Master pattern defintion for machine output digit patterns (in order)"
  [ " _     _  _     _  _  _  _  _ "
    "| |  | _| _||_||_ |_   ||_||_|"
    "|_|  ||_  _|  | _||_|  ||_| _|"] )

(def ^:const all-digkeys 
  "Master list of digit keywords (in order)"
  [ :zero :one :two :three :four :five :six :seven :eight :nine ] )

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

(defn parse-digits
  "Parse a set of 3 digit lines from the machine."
  [digit-lines]
  { :pre [(let [ [nrows ncols] (shape digit-lines) ]
             (and (= 3 nrows )             ; 3 lines
                  (= 0 (rem ncols 3)) )) ] ; multiple of 3
  }
  (->> digit-lines                  ; shape=[3 3n]   where n=num-digits
       (mapv #(partition 3 %) )     ; shape=[3 n 3]
       (apply mapv concat     ) ))  ; shape=[n 9]


(defn parse-entry
  "Parse an account number entry from the machine."
  [entry]
  { :pre [ (= [4 27] (shape entry) )  ; 4 lines, 27 char/line
           (apply = (into [\space] (last entry))) ] ; last line blank
    :post [ (= (shape %) [9 9] ) ] }
  (parse-digits (take 3 entry)) )

(defn digpats-to-lines
  "Format a sequence of digit patterns into 3 separate lines"
  [digpats] ; shape=[n 9]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats                       ; shape=[n 9]
       (mapv #(partition 3 %) )      ; shape=[n 3 3]
       (apply mapv concat     )      ; shape=[3n 3]
       (mapv str/join         )))    ; convert to string

(defn lines-to-str
  "Format a sequence of 3 lines into a single 3-line string (including newlines)."
  [lines]
  { :pre [ (= 3 (count lines)) ] }
  (str/join (flatten [ (interpose \newline  lines) ] )))

(defn digpats-to-str
  "Format a sequence of digit patterns into a single 3-line string."
  [digpats]
  { :pre [ (= 9 (second (shape digpats))) ] }
  (->> digpats
      (digpats-to-lines )
      (lines-to-str     ) ))

(def all-digpats    (parse-digits all-digit-lines))
(def digkey-digpat  (zipmap all-digkeys all-digpats ))

(defn digkey-to-lines
  "Covert a sequence of digit-key values into a 3-line digit pattern."
  [digkeys]
  (digpats-to-lines 
    (mapv digkey-digpat digkeys) ))

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  (log/msg "********************************************************************************")

  (assert (= (shape all-digit-lines) [3 30] ))
  (assert (= (shape all-digpats)     [10 9] ))

  (assert (=  (digpats-to-lines 
                (parse-digits 
                  (mapv #(take 18 %) all-digit-lines) ))
              [ " _     _  _     _ "
                "| |  | _| _||_||_ "
                "|_|  ||_  _|  | _|" ] ))

  (log/msg "digit patterns 2-5:" )
  (log/msg  (digpats-to-str 
              (parse-digits 
                (mapv #(->> % (drop  6 ) (take 12 ) ) all-digit-lines) )))

  (log/msg "all-digpats:" )
  (log/msg (lines-to-str (digpats-to-lines all-digpats) ))
  (assert (=  (digpats-to-lines all-digpats)
              [ " _     _  _     _  _  _  _  _ "
                "| |  | _| _||_||_ |_   ||_||_|"
                "|_|  ||_  _|  | _||_|  ||_| _|"] ))

  (log/msg "digpat ':three'")
  (log/msg [ (digkey-digpat :three) ] )

  (log/msg "digkeys 123" )
  (log/msg  (lines-to-str 
              (digkey-to-lines [ :one :two :three ]) ))

  (log/msg  (lines-to-str 
              (digkey-to-lines [ :one :two :three ]) ))

  (let [entry-1-9 [ "    _  _     _  _  _  _  _ "
                    "  | _| _||_||_ |_   ||_||_|"
                    "  ||_  _|  | _||_|  ||_| _|"
                    "                           " ]
        ent19-digpats (parse-entry entry-1-9) ]
    (log/msg)
    (log/msg "ent19-digpats")
    (log/msg (digpats-to-str ent19-digpats)) )
)

(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded

(defn -main [& args]
  (log/msg "Main program")
)

