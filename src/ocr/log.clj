(ns ocr.log
  (:require
    [clojure.string  :as str]
  ))

; Simple logging tools for demo. Replace with log4j or similar in production.

(def ^:const NEVER   99 )
(def ^:const FATAL    6 )
(def ^:const ERROR    5 )
(def ^:const WARN     4 )
(def ^:const MESSAGE  3 )  ; normal
(def ^:const EXTRA    2 )
(def ^:const DEBUG    1 )
(def ^:const TRACE    0 )

(def logging-min-level  MESSAGE )

(defn write-to-log
  "Write log msg to console for debugging."
  [level & msgs]
  (when (<= logging-min-level level)
    (apply println msgs ) ))

; Convenience functions
(defn fatal  [& msgs] (apply write-to-log  FATAL    msgs ))
(defn error  [& msgs] (apply write-to-log  ERROR    msgs ))
(defn warn   [& msgs] (apply write-to-log  WARN     msgs ))
(defn msg    [& msgs] (apply write-to-log  MESSAGE  msgs ))
(defn ext    [& msgs] (apply write-to-log  EXTRA    msgs ))
(defn dbg    [& msgs] (apply write-to-log  DEBUG    msgs ))
(defn trace  [& msgs] (apply write-to-log  TRACE    msgs ))


(defn set-min-level
  "Sets the minimum level for log messages to be reported."
  [level]
  (def logging-min-level level) )

(defn spy 
  "Prints label and arg to logging stream, then returns arg unaltered.  Useful for spying
  on contents of thread-last (->>) stream."
  [label arg]
  (do (msg label arg) arg) )

