(ns didactic-memory.combinators
  (:require [didactic-memory.primitives :as primitives]))

(defn alt
  ([p1 p2]
   (fn [inp]
     (into (p1 inp) (p2 inp))))
  ([p1 p2 & ps]
   (reduce alt (alt p1 p2) ps)))

(defn then [p1 p2]
  (fn [inp]
    (into []
          (comp
            (map (fn [[v1 out1]]
                   (map (fn [[v2 out2]]
                          [[v1 v2] out2])
                        (p2 out1))))
            cat)
          (p1 inp))))

(defn using [p f]
  (fn [inp]
    (into []
          (map (fn [[v out]]
                 [(f v) out])
               (p inp)))))

;; not working
(defn many [p]
  (alt
    (using
      (then
        (using
          (then p
                (alt p
                     (primitives/succeed [])))
          (fn [v]
            (reduce (fn [a b] (if (coll? b)
                                (into a b)
                                (conj a b)))
                    []
                    v)))
        (alt p
             (primitives/succeed [])))
      (fn [v]
        (reduce (fn [a b] (if (coll? b)
                            (into a b)
                            (conj a b)))
                []
                v)))
    (primitives/succeed [])))

(comment
  ((alt (primitives/literal \7)
        (primitives/satisfy number?)
        (primitives/satisfy char?)
        (primitives/literal \3))
   "345")
  ((then (primitives/literal \3)
         (primitives/satisfy char?))
   "345")
  ((many (primitives/literal \a))
   "aab"))
