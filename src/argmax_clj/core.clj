(ns argmax-clj.core)

(defn- pairs
  [[fst snd & rest]]
  (when (and fst snd)
    (conj (pairs rest) [fst snd])))

(defmacro argmax
  "Usage: (argmax seq-exprs body-expr)

   Repeatedly executes body-expr with bindings and filtering
   as provided by \"for\" to find the binding values at which body-expr
   reaches its max value. Returns a map containing the
   values of the bindings when body-expr is at max value. 

  (argmax [a (range 10) b (range 20)] (+ a b)) => {:a 9, :b 19}"
  [seq-exprs body-expr]
  (let [binding-pairs (pairs seq-exprs)
        iter-bindings (for [[binding _] binding-pairs
                             :when (not (keyword? binding))]
                        [(keyword binding) binding])
        let-bindings (for [[binding _] (flatten
                                        (filter (fn [[name value]]
                                                  (= name :let))
                                                 binding-pairs))]
                       [(keyword binding) binding])
        all-bindings (into iter-bindings let-bindings)]
    `(let [results# (for ~seq-exprs
                      (into {:result# ~body-expr}
                            ~(vec all-bindings)))]
       (dissoc (reduce (fn [x# y#]
                         (if (> (:result# x#) (:result# y#))
                           x#
                           y#))
                       results#)
               :result#))))

(defmacro argmin
  "Usage: (argmin seq-exprs body-expr)

   Repeatedly executes body-expr with bindings and filtering
   as provided by \"for\" to find the binding values at which body-expr
   reaches its min value. Returns a map containing the
   values of the bindings when body-expr is at min value. 

  (:a (argmin [a (range 10) b (range 20)] (+ a b))) => {:a 0, :b 0}"
    [seq-exprs body-expr]
  (let [binding-pairs (pairs seq-exprs)
        iter-bindings (for [[binding _] binding-pairs
                             :when (not (keyword? binding))]
                        [(keyword binding) binding])
        let-bindings (for [[binding _] (flatten
                                        (filter (fn [[name value]]
                                                  (= name :let))
                                                 binding-pairs))]
                       [(keyword binding) binding])
        all-bindings (into iter-bindings let-bindings)]
    `(let [results# (for ~seq-exprs
                      (into {:result# ~body-expr}
                            ~(vec all-bindings)))]
       (dissoc (reduce (fn [x# y#]
                         (if (< (:result# x#) (:result# y#))
                           x#
                           y#))
                       results#)
               :result#))))

