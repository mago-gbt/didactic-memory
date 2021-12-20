(ns didactic-memory.misc
  (:require [didactic-memory.primitives :as prim]
            [didactic-memory.combinators :as com]))

(defn pos [inp & {:keys [row-change-pred]}]
  (let [row-change-pred (or row-change-pred #(= % \newline))]
    (->> inp
         (reduce (fn [acc c]
                   (let [change-row? (row-change-pred c)
                         row (or (:row acc) 0)
                         col (or (:col acc) 0)]
                     (-> acc
                         (update :res conj [c [row col]])
                         (assoc :row (if change-row? (inc row) row)
                                :col (if change-row? 0 (inc col))))))
                 {:res []})
         :res)))


(comment
  (pos "some test \n string")
  (pos "some test
        multiline
        string"))
