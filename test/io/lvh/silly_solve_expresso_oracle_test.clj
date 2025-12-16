(ns io.lvh.silly-solve-expresso-oracle-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [io.lvh.silly-solve :as ss]
   [io.lvh.silly-solve-test-util :refer [approximately=]]
   [numeric.expresso.core :as ex]
   [clojure.walk :as walk]))

;; =============================================================================
;; Expresso Integration Utilities
;; =============================================================================

(defn keyword->symbol
  "Convert a keyword to a symbol. Expresso works better with symbols for multi-variable solving."
  [k]
  (if (keyword? k)
    (symbol (name k))
    k))

(defn symbol->keyword
  "Convert a symbol back to a keyword for silly-solve compatibility."
  [s]
  (if (symbol? s)
    (keyword (name s))
    s))

(defn convert-expr-keywords->symbols
  "Recursively convert all keywords in an expression to symbols."
  [expr]
  (walk/postwalk
   (fn [x]
     (if (keyword? x)
       (keyword->symbol x)
       x))
   expr))

(defn convert-result-symbols->keywords
  "Convert symbol keys in a result map back to keywords."
  [result-map]
  (when (map? result-map)
    (into {} (map (fn [[k v]] [(symbol->keyword k) v]) result-map))))

(defn extract-variables-from-equations
  "Extract all variables from a list of equations."
  [equations]
  (let [operators #{'+ '- '* '/ '** 'max 'min '=}]
    (->> equations
         (mapcat (fn [eq] (if (and (seq? eq) (= (first eq) '=))
                           (rest eq)
                           [])))
         (mapcat #(tree-seq seq? identity %))
         (filter ss/variable?)
         (remove operators)
         (into #{}))))

(defn try-expresso-solve
  "Attempt to solve equations with expresso. Returns result or nil if it fails.
   Converts keywords to symbols since expresso handles symbols better for multi-variable solving."
  [equations]
  (try
    (let [;; Convert keywords to symbols for expresso compatibility
          symbol-equations (map convert-expr-keywords->symbols equations)
          variables (vec (extract-variables-from-equations symbol-equations))]
      (when (seq variables)
        ;; Build expresso expressions and solve
        (let [ex-equations (map #(eval `(ex/ex ~%)) symbol-equations)
              result (apply ex/solve variables ex-equations)]
          ;; Filter out empty solutions
          (when (and result
                     (seq result)
                     (not (contains? result {})))
            result))))
    (catch Exception e
      nil)))

(defn expresso-result->silly-solve
  "Convert expresso's result to silly-solve's constant map format.
   Expresso can return:
   - #{3} for single variable solutions
   - #{{x 3, y 2}} for system solutions
   Converts symbol keys back to keywords for silly-solve compatibility."
  [expresso-result variables]
  (when (and expresso-result (seq expresso-result))
    (let [first-result (first expresso-result)]
      (cond
        ;; Single number result for single variable equations
        (number? first-result)
        (when (= 1 (count variables))
          {(symbol->keyword (first variables)) first-result})

        ;; Map result for system equations - convert symbol keys to keywords
        (map? first-result)
        (when (seq first-result)
          (convert-result-symbols->keywords first-result))

        :else nil))))

(defn solutions-equivalent?
  "Check if two solution maps are equivalent, handling floating point precision."
  [silly-solve-result expresso-result variables]
  (when (and silly-solve-result expresso-result)
    (let [silly-constants (second silly-solve-result)  ; silly-solve returns [remaining-eqns constants]
          expresso-constants (expresso-result->silly-solve expresso-result variables)]
      (when (and silly-constants expresso-constants
                 (map? silly-constants) (map? expresso-constants))
        (let [silly-vars (set (keys silly-constants))
              expresso-vars (set (keys expresso-constants))]
          (and (= silly-vars expresso-vars)
               (every? (fn [var]
                         (let [silly-val (get silly-constants var)
                               expresso-val (get expresso-constants var)]
                           (and silly-val expresso-val
                                (approximately= silly-val expresso-val))))
                       silly-vars)))))))

;; =============================================================================
;; Generators for Intersection Operations
;; =============================================================================

(def gen-variable
  "Generator for variables from a fixed set."
  (gen/elements [:a :b :c :x :y :z]))

(def gen-rational
  "Generator for rational numbers."
  (gen/fmap (fn [[n d]] (/ n d))
            (gen/tuple (gen/choose -10 10)
                       (gen/fmap inc (gen/choose 0 9)))))

(def gen-nonzero-rational
  "Generator for non-zero rationals."
  (gen/such-that (complement zero?) gen-rational))

(def gen-leaf
  "Generator for expression leaves (constants or variables)."
  (gen/one-of [gen-rational gen-variable]))

(def gen-binary-op
  "Generator for binary operations both solvers support."
  (gen/elements ['+ '- '* '/]))

(def gen-commutative-op
  "Generator for commutative operations both solvers support."
  (gen/elements ['+ '* 'max 'min]))

(defn gen-expr
  "Recursive generator for expressions up to given depth.
   Used for testing oracle agreement on moderately complex expressions."
  [max-depth]
  (if (<= max-depth 0)
    gen-leaf
    (gen/frequency
     [[3 gen-leaf]  ; Bias toward leaves to avoid explosion
      [1 (gen/let [op gen-binary-op
                   left (gen-expr (dec max-depth))
                   right (gen-expr (dec max-depth))]
           (list op left right))]
      [1 (gen/let [op gen-commutative-op
                   args (gen/vector (gen-expr (dec max-depth)) 2 3)]
           (cons op args))]])))

(def gen-moderate-expr
  "Generator for moderately complex expressions (depth 3)."
  (gen-expr 3))

(def gen-simple-solvable-system
  "Generator for simple systems both solvers should handle."
  (gen/let [var1 (gen/elements [:a :b])
            const1 gen-nonzero-rational
            var2 (gen/elements [:x :y])
            op (gen/elements ['+ '*])
            const2 gen-nonzero-rational]
    [(list '= var1 const1)
     (list '= var2 (list op var1 const2))]))

;; =============================================================================
;; Oracle Comparison Tests
;; =============================================================================

(defn oracle-result
  "Generate property result for oracle comparison tests."
  [pass? data]
  (if pass?
    true
    (do
      (println "\n=== ORACLE DISAGREEMENT DIAGNOSTICS ===")
      (println "Equations:" (:equations data))
      (println "Silly-solve result:" (:silly-result data))
      (println "Expresso result:" (:expresso-result data))
      (let [variables (:variables data)
            silly-constants (when (:silly-result data) (second (:silly-result data)))
            expresso-constants (when (:expresso-result data)
                                (expresso-result->silly-solve (:expresso-result data) variables))]
        (println "Silly-solve constants:" silly-constants)
        (println "Expresso constants:" expresso-constants))
      (when-let [error (:error data)]
        (println "Error:" error))
      (println "=== END DIAGNOSTICS ===\n")
      false)))

(defspec oracle-agrees-on-simple-systems 100
  (prop/for-all [equations gen-simple-solvable-system]
    (let [variables (vec (extract-variables-from-equations equations))
          silly-result (try (ss/solve-for-consts equations) (catch Exception e nil))
          expresso-result (try-expresso-solve equations)
          silly-constants (when silly-result (second silly-result))
          expresso-constants (when expresso-result (expresso-result->silly-solve expresso-result variables))

          both-solved? (and silly-result expresso-result
                           (empty? (first silly-result)) ; silly-solve fully solved
                           )
          both-failed? (and (nil? silly-result) (nil? expresso-result))
          silly-stuck? (and silly-result (seq (first silly-result))) ; silly-solve has remaining equations
          expresso-stuck? (and (not expresso-result) silly-result)

          pass? (cond
                  both-failed? true  ; Both failed, that's fine
                  both-solved? (solutions-equivalent? silly-result expresso-result variables)
                  silly-stuck? true  ; Expected - silly-solve is simpler (separate metric)
                  expresso-stuck? true ; Unexpected but not a disagreement
                  :else false)]  ; One succeeded, other failed unexpectedly

      (oracle-result
       pass?
       {:equations equations
        :variables variables
        :silly-result silly-result
        :expresso-result expresso-result
        :both-solved? both-solved?
        :both-failed? both-failed?
        :silly-stuck? silly-stuck?}))))

;; =============================================================================
;; Single Equation Oracle Tests
;; =============================================================================

(def gen-single-simple-equation
  "Generator for single equations both solvers should handle."
  (gen/let [var gen-variable
            op (gen/elements ['+ '*])
            const1 gen-nonzero-rational
            const2 gen-nonzero-rational]
    (list '= var (list op const1 const2))))

(defspec oracle-agrees-on-single-equations 100
  (prop/for-all [equation gen-single-simple-equation]
    (let [equations [equation]
          variables (vec (extract-variables-from-equations equations))
          silly-result (try (ss/solve-for-consts equations) (catch Exception e nil))
          expresso-result (try-expresso-solve equations)

          both-solved? (and silly-result expresso-result
                           (empty? (first silly-result)))

          pass? (if both-solved?
                  (solutions-equivalent? silly-result expresso-result variables)
                  true)]  ; If not both solved, don't compare (separate metric)

      (oracle-result
       pass?
       {:equations equations
        :variables variables
        :silly-result silly-result
        :expresso-result expresso-result}))))

;; =============================================================================
;; Moderate Complexity Expression Tests
;; =============================================================================

(def gen-moderate-equation
  "Generator for equations with moderately complex RHS expressions."
  (gen/let [var gen-variable
            expr gen-moderate-expr]
    (list '= var expr)))

(defn evaluate-expr
  "Evaluate an expression with given variable bindings.
   Returns nil if evaluation fails (e.g., division by zero, unbound vars)."
  [expr bindings]
  (try
    (cond
      (number? expr) expr
      (ss/variable? expr) (get bindings expr)
      (seq? expr)
      (let [[op & args] expr
            evaluated-args (map #(evaluate-expr % bindings) args)]
        (when (every? some? evaluated-args)
          (case op
            + (apply + evaluated-args)
            - (apply - evaluated-args)
            * (apply * evaluated-args)
            / (when (every? #(not (zero? %)) (rest evaluated-args))
                (apply / evaluated-args))
            max (apply max evaluated-args)
            min (apply min evaluated-args)
            nil)))
      :else nil)
    (catch Exception _ nil)))

(defspec oracle-agrees-on-moderate-expressions 100
  (prop/for-all [equation gen-moderate-equation]
    (let [equations [equation]
          variables (vec (extract-variables-from-equations equations))
          silly-result (try (ss/solve-for-consts equations) (catch Exception e nil))
          expresso-result (try-expresso-solve equations)

          ;; Both solved successfully?
          both-solved? (and silly-result expresso-result
                           (empty? (first silly-result)))

          ;; If both solved, compare results
          pass? (cond
                  ;; Both solved - verify they agree
                  both-solved?
                  (solutions-equivalent? silly-result expresso-result variables)

                  ;; Expresso solved but silly-solve stuck - that's OK (silly-solve is simpler)
                  (and expresso-result silly-result (seq (first silly-result)))
                  true

                  ;; Neither solved or only one solved - acceptable for complex expressions
                  :else true)]

      (oracle-result
       pass?
       {:equations equations
        :variables variables
        :silly-result silly-result
        :expresso-result expresso-result
        :both-solved? both-solved?}))))

;; =============================================================================
;; Coverage Analysis Tests
;; =============================================================================

(defn track-solver-outcomes
  "Track different outcomes when comparing solvers.
   This provides separate metrics as requested."
  [equations]
  (let [silly-result (try (ss/solve-for-consts equations) (catch Exception e nil))
        expresso-result (try-expresso-solve equations)]
    {:both-solved (and silly-result expresso-result
                      (empty? (first silly-result)))
     :both-failed (and (nil? silly-result) (nil? expresso-result))
     :silly-stuck (and silly-result (seq (first silly-result)))
     :expresso-only (and expresso-result (or (nil? silly-result)
                                            (seq (first silly-result))))
     :silly-only (and silly-result (empty? (first silly-result)) (nil? expresso-result))}))

;; Test to gather metrics on solver coverage - not for pass/fail but for analysis
(defspec solver-coverage-analysis 50
  (prop/for-all [equations gen-simple-solvable-system]
    (let [outcomes (track-solver-outcomes equations)]
      ;; Always pass - this is just for gathering data
      (when (or (:expresso-only outcomes) (:silly-only outcomes))
        (println "Coverage difference detected:" outcomes "for equations:" equations))
      true)))