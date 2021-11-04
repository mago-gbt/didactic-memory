(ns didactic-memory.combinators
  (:require [didactic-memory.primitives :as prim]))

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

(defn xthen [p1 p2]
  (using (then p1 p2) second))

(defn thenx [p1 p2]
  (using (then p1 p2) first))

(defn many [p]
  (fn [inp]
    (let [inner-p (alt
                    (using
                      (then p (many p))
                      #(apply cons %))
                    (prim/succeed []))]
      (inner-p inp))))

(defn some [p]
  (fn [inp]
    (let [inner-p (using
                    (then p (many p))
                    #(apply cons %))]
      (inner-p inp))))

(defn string [s]
  (fn [inp]
    (let [inner-p (cond (empty? s) (prim/succeed [])
                        :else (let [[x & xs] s]
                                (using (then (prim/literal x)
                                             (string xs))
                                       #(apply cons %))))]
      (inner-p inp))))

(defn return [p v]
  (using p
         (fn [x]
           v)))

(comment
  ((alt (primitives/literal \7)
        (primitives/satisfy number?)
        (primitives/satisfy char?)
        (primitives/literal \3))
   "345")
  ((then (primitives/literal \3)
         (primitives/satisfy char?))
   "345")
  (many (primitives/literal \a))
  ((some (primitives/literal \a))
   "b")
  ((string "someword")
   "someword someotherword"))
