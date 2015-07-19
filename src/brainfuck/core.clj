(ns brainfuck.core
  (:require [clojure.string :as str]))

(def initial-state [[] 0 (list)])
(defn zero-if-nil [x] (or x 0))

(defn make-parser
  ([] (make-parser initial-state))
  ([[prev cur after]]
    (fn ([] [prev cur after])
      ([c] (condp = c
            \< [(butlast prev) (zero-if-nil (last prev)) (conj after cur)]
            \> [(conj prev cur) (zero-if-nil (first after)) (next after)]
            \- [prev (dec cur) after]
            \+ [prev (inc cur) after]
            \. (do (print (char cur)) [prev cur after])
            \, [prev (.read System/in) after]
            c  [prev cur after])))))

(defn parse-expression
  ([s]
  (parse-expression (make-parser) s))
  ([parser s]
  (cond (= (first s) \[)
    (loop [p parser]
      (let [[states next-s] (parse-expression p (next s))]
        (if-not (= 0 (second states))
          (recur (make-parser states))
          (parse-expression (make-parser states) next-s))))
    (or (empty? s) (= (first s) \]))
      [(parser) (next s)]
    :else
      (parse-expression (make-parser (parser (first s)))
                         (next s)))))

(defn -main []
  (println "Starting the magnificent BrainFuck REPL ... ")
    (print ">> ")
    (flush)
    (dorun (reduce (fn [state expr]
                     (let [next-state (first (parse-expression
                                            (make-parser state) expr))]
                       (println "=>" (second next-state))
                       (print ">> ")
                       (flush)
                       next-state))
                   initial-state (line-seq (java.io.BufferedReader. *in*)))))
