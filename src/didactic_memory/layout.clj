(ns didactic-memory.layout
  (:require [didactic-memory.primitives :as prim]
            [didactic-memory.combinators :as com]))

(def white
  (com/many (com/any prim/literal " \t\n")))

(defn nibble [p]
  (com/xthen white
             (com/thenx p
                        white)))

(def symbol-p
  (comp nibble com/string))

(defn offside [p]
  (fn [inp]
    (let [onside (fn [[_ r1 c1] [_ r2 c2]]
                   (and (>= r2 r1)
                        (>= c2 c1)))
          inp-on (take-while (partial onside (first inp))
                             inp)
          inp-off (drop (count inp-on) inp)]
      (for [[v _] (p inp-on)]
        [v inp-off]))))
