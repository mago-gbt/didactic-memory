(ns didactic-memory.type-preds
  (:require [didactic-memory.primitives :as prim]
            [didactic-memory.combinators :as com]))

(defn digit [c]
  (<= (int \0) (int c) (int \9)))

(defn letter [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))))

(def number
  (com/some (prim/satisfy digit)))

(def word
  (com/some (prim/satisfy letter)))


(comment
  (word "blah"))
