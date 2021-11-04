(ns didactic-memory.primitives)

(defn succeed [v]
  (fn [inp]
    [[v inp]]))

(def fail
  (fn [inp]
    []))

(defn satisfy [pred]
  (fn [inp]
    (let [[x & xs] inp]
      (cond
        (empty? inp) (fail inp)
        :else (cond
                (pred x) ((succeed x) xs)
                :else (fail xs))))))

(defn literal [x]
  (satisfy #(= x %)))

(comment
  ((literal \3) "345"))
