(ns io.lvh.silly-solve
  (:require
    #?@(:clj
          [[meander.strategy.epsilon :as r]
           [meander.epsilon :as m]
           [clojure.math.numeric-tower :refer [expt]]]
        :cljs
        [[meander.strategy.epsilon :as r :include-macros true]
         [meander.epsilon :as m :include-macros true]]))
  #?(:clj (:gen-class)))

(def variable? (some-fn symbol? keyword?))

(def ^:private ops
  [{::symbol '+ ::fn + ::commutative true}
   {::symbol '* ::fn * ::commutative true}
   {::symbol '- ::fn - ::invertible-with '+}
   {::symbol '/ ::fn / ::invertible-with '*}
   {::symbol '** ::fn #?(:clj expt :cljs #(.pow js/Math %1 %2))}
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

(defn ^:private equality-const-to-front
  "Merge constants in an equality into 1 constant and put it up front."
  [args]
  (let [{consts true exprs+vars false} (group-by number? args)]
    (assert (apply = consts) "impossible system of equations, no solutions")
    (apply list '= (first consts) exprs+vars)))

(def invertible-ops
  (->> (m/search ops
         (m/scan {::symbol ?sym ::invertible-with (m/some ?inv)})
         [?sym ?inv])
       (into {})))

(defn ^:private invert
  [op-sym [x & ys]]
  (if (empty? ys)
    (list op-sym x)  ; Return unary op unchanged, e.g. (- x) stays (- x)
    (let [inverted (invertible-ops op-sym)]
      (->> (map (fn [y] (list op-sym y)) ys)
           (apply list inverted x)))))

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
     ('** 1 ?x) ?x ;; unquoted `**` would be interpreted as memory variable `*`

     ;; Remove tautologies & meaningless unary equalities
     (= & (m/pred (fn [args] (or (empty? args) (apply = args))) ?args)) nil

     ;; Evaluate expressions of an op with a bunch of constants:
     ((m/pred op-sym? ?op) . (m/number !consts) ...)
     (m/app apply-op ?op [!consts ...])

     ((m/pred commutative-ops ?op) & ?args)
     (m/app commutative-simplify ?op ?args)

     ((m/pred invertible-ops ?op) & ?args)
     (m/app invert ?op ?args)

     ;; Move constants to the front of equations
     (= & (m/pred (fn [args] (->> args rest (some number?))) ?args))
     (m/app equality-const-to-front ?args))
   (r/attempt)
   (r/bottom-up)
   (r/until =)))

(defn ^:private find-consts
  [eqns]
  ;; Note we assume simplification already happened and so all consts follow all
  ;; vars-or-equations.
  (->>
   (for [[equals-sign const & args] eqns
         :when (= '= equals-sign)
         :when (number? const)
         var (filter variable? args)]
     [var const])
   (into {})))

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
