(ns didactic-memory.lexing
  (:require [didactic-memory.primitives :as prim]
            [didactic-memory.combinators :as com]
            [didactic-memory.type-preds :as type-preds]
            [didactic-memory.misc :as misc]
            [clojure.core.reducers :as r]))

(defn tok [p tag]
  (fn [inp]
    (let [[_ [r c]] (first inp)]
      (->> (p inp)
           (r/map (fn [[xs out]]
                    [[[tag xs] [r c]] out]))
           (r/fold (fn ([] [])
                       ([acc r] (into acc r))))))))

(defn lex [pts]
  (println "pts: ")
  (println pts)
  (println (map (fn [[p t]]
                  (tok p t))
                pts))
  (com/many (apply com/alt
                   (map (fn [[p t]]
                          (tok p t))
                        pts))))


(def lexer (lex [[(com/some-p (com/any prim/literal " \t\n")) :junk]
                 [(com/string "where") :symbol]
                 [type-preds/word :ident]
                 [type-preds/number :number]
                 [(com/any com/string ["(" ")" "="]) :symbol]]))

(first (lexer (misc/pos "f x y = add a b
                                 where
                                    a = 25
                                    b = sub x y
                         answer = mult (f 3 7) 5")))

(comment

  (lexer (misc/pos "f x y = add a b
                            where
                                a = 25
                                b = sub x y
                    answer = mult (f 3 7) 5")))
