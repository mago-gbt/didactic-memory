(ns didactic-memory.combinators
  (:require [didactic-memory.primitives :as prim]
            [clojure.core.reducers :as r]))

(defn alt
  ([p1 p2 & ps]
   (fn [inp]
     (->> (into [p1 p2] ps)
          (r/map (fn [p] (p inp)))
          (r/fold (fn ([] [])
                      ([acc r]
                       (into acc r))))))))

(defn then [p1 p2]
  (fn [inp]
    (->> (p1 inp)
         (r/map (fn [[v1 out1]]
                  (->> (p2 out1)
                       (r/map (fn [[v2 out2]]
                                [[v1 v2] out2]))
                       (r/fold (fn ([] [])
                                   ([acc r]))))))
         (r/fold (fn ([] [])
                     ([acc r]))))))

(defn using [p f]
  (fn [inp]
    (->> (p inp)
         (r/map (fn [[v out]]
                  [(f v) out]))
         (r/fold (fn ([] [])
                     ([acc r]
                      (into acc r)))))))

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

(defn some-p [p]
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

(defn any [p xs]
  (apply alt (map p xs)))

(comment ((any prim/literal " \t\n")
          (didactic-memory.misc/pos " ")))

(comment (didactic-memory.misc/pos " "))

(comment

  ((alt (prim/literal \3)
        (prim/satisfy char?))
   (didactic-memory.misc/pos "345"))
  ((then (primitives/literal \3)
         (primitives/satisfy char?))
   "345")
  (many (primitives/literal \a))
  ((some (primitives/literal \a))
   "b")
  ((string "someword")
   "someword someotherword"))
