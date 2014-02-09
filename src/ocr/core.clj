(ns ocr.core
  (:require
    [clojure.string          :as str]
    [clojure.set             :as set]
    [clojure.core.incubator  :as cci]
    [ocr.log                 :as log]
  ))

(log/set-min-level log/DEBUG)

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

(def digit-patterns [ " _     _  _     _  _  _  _  _ "
                      "| |  | _| _||_||_ |_   ||_||_|"
                      "|_|  ||_  _|  | _||_|  ||_| _|" ] )

(defn do-tests 
  "Documents (& tests) regex stuff."
  []
  ; Break down the digits pattern sequence into individual 9-char vectors.

  (log/dbg "digits-to-str")
  (log/dbg (digits-to-str [ "abcdefghi" "123456789" ] ))

  (def dps1 digit-patterns )
  (assert (= (shape digit-patterns) [3 30] )) ; [line all-digit-chars]

  (log/dbg)
  (log/dbg "digit-patterns:" )
  (doseq [ line dps1 ] (log/dbg line) )

  (def dps2 (map #(partition 3 %) dps1 ))
  (assert (= (shape dps2) [3 10 3] ))  ; [line digit char]

  (log/dbg)
  (log/dbg "dps2:" (shape dps2) )
  (doseq [line dps2] 
    (doseq [digit line]
      (log/dbg (str/join (flatten [\" digit "\" "] )))
    ))

  (def dps3 (map vec (apply map concat dps2)))
  (assert (= (shape dps3) [10 9] ))  ; [digit all-chars]

  (log/dbg)
  (log/dbg "dps3:" (shape dps3))
  (doseq [digit dps3]
    (log/dbg ) 
    (log/dbg digit)
    (log/dbg (digit-to-str digit)) 
  )

  (log/dbg "All digits:")
  (log/dbg (digits-to-str dps3 ))

)
(defonce test-results (do-tests) )  ; Ensure tests run once when code loaded


(defn -main [& args]
  (log/dbg "Main program")
)

