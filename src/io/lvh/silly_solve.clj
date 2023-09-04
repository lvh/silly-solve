(ns io.lvh.silly-solve
  (:require
   [meander.strategy.epsilon :as r]
   [meander.epsilon :as m]
   [clojure.math.numeric-tower :refer [expt]])
  (:gen-class))

(def variable? (some-fn symbol? keyword?))

(def ^:private ops
  [{::symbol '+ ::fn + ::commutative true}
   {::symbol '* ::fn * ::commutative true}
   {::symbol '- ::fn - ::invertible-with '+}
   {::symbol '/ ::fn / ::invertible-with '*}
   {::symbol '** ::fn expt}
   {::symbol 'max ::fn max ::commutative true}
   {::symbol 'min ::fn min ::commutative true}])

(def ^:private op-sym?
  (into #{} (map ::symbol) ops))

(def commutative-ops
  (->> (m/search ops
         (m/scan {::symbol ?sym ::fn ?fn ::commutative true})
         [?sym ?fn])
       (into {})))

(defn ^:private commutative-simplify
  [op-sym args]
  (let [{consts true vars false} (group-by number? args)
        folded-const (-> op-sym commutative-ops (apply consts))]
    `(~op-sym ~folded-const ~@vars)))

(def invertible-ops
  (->> (m/search ops
         (m/scan {::symbol ?sym ::invertible-with (m/some ?inv)})
         [?sym ?inv])
       (into {})))

(defn ^:private invert
  [op-sym [x & ys]]
  (let [inverted (invertible-ops op-sym)]
    (->> (map (fn [y] (list op-sym y)) ys)
         (apply list inverted x))))

(defn ^:private apply-op
  [op-sym args]
  (let [f (m/find ops (m/scan {::symbol ~op-sym ::fn ?f}) ?f)]
    (apply f args)))

(def simplify
  (->>
   (r/rewrite
    ;; Resolve unary ops. These don't happen in human-written equations but they
    ;; happen all the time in generated ones.
    (+ ?x) ?x
    (* ?x) ?x
    ('max ?x) ?x
    ('min ?x) ?x

    ;; Maybe these neutral elements should be properties of the ops (maybe even
    ;; automatically generated since generally `(op)` will return the neutral
    ;; element!) but honestly just writing these as rewrite rules is so trivial
    ;; I didn't bother.
    (+ 0 ?x) ?x
    (* 1 ?x) ?x
    ('** 1 ?x) ?x ;; ** would be a memory variable named '*'... 😬

    ;; Remove tautologies:
    (= ?x ?x) nil

    ((m/pred op-sym? ?op) . (m/number !consts) ...)
    (m/app apply-op ?op [!consts ...])

    ((m/pred commutative-ops ?op) & ?args)
    (m/app commutative-simplify ?op ?args)

    ((m/pred invertible-ops ?op) & ?args)
    (m/app invert ?op ?args))
   (r/attempt)
   (r/bottom-up)
   (r/until =)))

(defn ^:private find-consts
  [eqns]
  (->>
   (m/search
    eqns
    (m/scan (= (m/pred variable? ?var) (m/number ?val)))
    [?var ?val])
   (into {})))

(defn ^:private propagate-consts
  [eqns consts]
  (m/find eqns
    (m/$ ?ctx (m/pred consts ?const))
    (?ctx (consts ?const))))

(defn ^:private propagate-consts
  [eqns consts]
  (let [strategy
        (->>
         (r/rewrite
          (m/pred consts ?x)
          (m/app consts ?x))
         (r/attempt)
         (r/bottom-up)
         (r/until =))]
    (strategy eqns)))

(defn solve-for-consts
  ([eqns]
   (solve-for-consts eqns {}))
  ([eqns consts]
   (loop [eqns eqns
          consts consts]
     (let [new-consts (->> eqns (find-consts) (merge consts))
           new-eqns (->> new-consts
                         (propagate-consts eqns)
                         (simplify)
                         (remove nil?))
           solved? (empty? new-eqns)
           stuck? (and (= consts new-consts)
                       (= eqns new-eqns))]
       (if (or solved? stuck?)
         [new-eqns new-consts]
         (recur new-eqns new-consts))))))
