(ns didactic-memory.primitives)

(defn succeed [v]
  (fn [inp]
    [[v inp]]))

(def fail
  (fn [inp]
    []))

(defn satisfy [pred]
  (fn [inp]
    (let [[x & xs] inp
          [a [r c]] x]
      (cond
        (empty? inp) (fail inp)
        :else (cond
                (pred a) ((succeed a) xs)
                :else (fail xs))))))

(defn literal [x]
  (satisfy #(= x %)))

(comment ((literal \3) (didactic-memory.misc/pos "335")))

(comment

  ((literal 3) [3 4 5]))
