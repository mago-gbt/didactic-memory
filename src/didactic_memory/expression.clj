(ns didactic-memory.expression
  (:require [didactic-memory.primitives :as prim]
            [didactic-memory.combinators :as com]
            [didactic-memory.type-preds :as tpreds]))

(declare expn)
(declare term)
(declare factor)

(def factor
  (fn [inp]
    (let [p
          (com/alt
            (com/using tpreds/number
                       #(read-string (apply str %)))
            (com/xthen (prim/literal (char 40))
                       (com/thenx expn
                                  (prim/literal (char 41)))))]
      (p inp))))

(def term
  (fn [inp]
    (let [p
          (com/alt
            (com/using (com/then factor
                                 (com/xthen
                                           (prim/literal \*)
                                           factor))
                       #(apply * %))
            (com/using (com/then factor
                                 (com/xthen
                                           (prim/literal \/)
                                           factor))
                       #(apply / %))
            factor)]
      (p inp))))

(def expn
  (fn [inp]
    (let [p
          (com/alt (com/using (com/then term
                                        (com/xthen
                                                  (prim/literal \+)
                                                  term))
                              #(apply + %))
                   (com/using (com/then term
                                        (com/xthen
                                                  (prim/literal \-)
                                                  term))
                              #(apply - %))
                  term)]
      (p inp))))

(comment
  (expn "2+(4-1)*3"))
