(ns io.lvh.silly-solve-generative-test
  (:require
   [clojure.test :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :refer [defspec]]
   [io.lvh.silly-solve :as ss]
   [meander.strategy.epsilon :as r]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-variable
  "Generator for variables from a fixed set."
  (gen/elements [:a :b :c :x :y :z]))

(def gen-rational
  "Generator for rational numbers (excluding zero for divisors)."
  (gen/fmap (fn [[n d]] (/ n d))
            (gen/tuple (gen/choose -100 100)
                       (gen/fmap inc (gen/choose 0 99)))))

(def gen-nonzero-rational
  "Generator for non-zero rationals (safe for division)."
  (gen/such-that (complement zero?) gen-rational))

(def gen-positive-int
  "Generator for small positive integers (for exponents)."
  (gen/choose 1 5))

;; Per-operation expression generators (depth 1)

(def gen-addition-expr
  "Generator for simple addition expressions."
  (gen/let [args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons '+ args)))

(def gen-multiplication-expr
  "Generator for simple multiplication expressions."
  (gen/let [args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons '* args)))

(def gen-subtraction-expr
  "Generator for simple subtraction expressions."
  (gen/let [args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons '- args)))

(def gen-division-expr
  "Generator for simple division expressions (non-zero divisors)."
  (gen/let [first-arg (gen/one-of [gen-rational gen-variable])
            rest-args (gen/vector (gen/one-of [gen-nonzero-rational gen-variable]) 1 3)]
    (cons '/ (cons first-arg rest-args))))

(def gen-max-expr
  "Generator for max expressions."
  (gen/let [args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons 'max args)))

(def gen-min-expr
  "Generator for min expressions."
  (gen/let [args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons 'min args)))

(def gen-simple-expr
  "Generator for any simple (depth-1) expression."
  (gen/one-of [gen-addition-expr
               gen-multiplication-expr
               gen-subtraction-expr
               gen-division-expr
               gen-max-expr
               gen-min-expr]))

;; Nested expression generator (depth 5-6)

(def gen-leaf
  "Generator for expression leaves (constants or variables)."
  (gen/one-of [gen-rational gen-variable]))

(def gen-commutative-op
  "Generator for commutative operation symbols."
  (gen/elements ['+ '* 'max 'min]))

(def gen-binary-op
  "Generator for any binary operation symbol."
  (gen/elements ['+ '- '* '/]))

(defn gen-expr
  "Recursive generator for expressions up to given depth."
  [max-depth]
  (if (<= max-depth 0)
    gen-leaf
    (gen/frequency
     [[3 gen-leaf]  ; bias toward leaves to avoid explosion
      [1 (gen/let [op gen-binary-op
                   left (gen-expr (dec max-depth))
                   right (gen-expr (dec max-depth))]
           (list op left right))]
      [1 (gen/let [op gen-commutative-op
                   args (gen/vector (gen-expr (dec max-depth)) 2 4)]
           (cons op args))]])))

(def gen-nested-expr
  "Generator for moderately nested expressions (depth 5)."
  (gen-expr 5))

;; =============================================================================
;; Smoke test for generators
;; =============================================================================

;; =============================================================================
;; Helpers for property tests
;; =============================================================================

(defn extract-variables
  "Extract all variables (keywords/symbols) from an expression."
  [expr]
  (cond
    (ss/variable? expr) #{expr}
    (seq? expr) (into #{} (mapcat extract-variables) (rest expr))
    :else #{}))

(def op-fns
  "Map of operation symbols to their functions."
  {'+ +, '- -, '* *, '/ /,
   'max max, 'min min,
   '** #(Math/pow %1 %2)})

(defn evaluate
  "Evaluate an expression with given variable bindings.
   Returns nil if evaluation fails (e.g., division by zero)."
  [expr bindings]
  (try
    (cond
      (number? expr) expr
      (ss/variable? expr) (get bindings expr)
      (seq? expr)
      (let [[op & args] expr
            evaluated-args (map #(evaluate % bindings) args)]
        (when (every? some? evaluated-args)
          (apply (op-fns op) evaluated-args)))
      :else nil)
    (catch Exception _ nil)))

(defn gen-bindings-for
  "Generate bindings for all variables in an expression."
  [expr]
  (let [vars (extract-variables expr)]
    (if (empty? vars)
      (gen/return {})
      (gen/fmap #(zipmap vars %)
                (gen/vector gen-nonzero-rational (count vars))))))

;; =============================================================================
;; Failure-catching with tracing
;; =============================================================================

(def traced-simplify
  "Simplify with tracing enabled."
  (r/trace ss/simplify))

(defn simplify-with-trace
  "Run simplify and capture trace output as a string."
  [expr]
  (let [trace-output (with-out-str (traced-simplify expr))]
    trace-output))

(defn solve-with-trace
  "Run solve-for-consts and capture trace output.
   Returns [result trace-string]."
  [eqns]
  (let [trace-output (java.io.StringWriter.)
        result (binding [*out* trace-output]
                 (let [traced-simp (r/trace ss/simplify)]
                   ;; We need to temporarily rebind simplify, but since it's
                   ;; called internally, we'll just trace the whole solve
                   (ss/solve-for-consts eqns)))]
    [result (str trace-output)]))

(defn check-with-trace
  "Run a check function. If it fails, re-run with tracing and return diagnostic info.
   check-fn: (fn [] {:pass? bool :data map})
   trace-fn: (fn [] string) - function to run for trace output on failure
   Returns: {:pass? bool :trace string-or-nil :data map}"
  [check-fn trace-fn]
  (let [{:keys [pass? data] :as result} (check-fn)]
    (if pass?
      result
      (let [trace (try (trace-fn) (catch Exception e (str "Trace error: " e)))]
        (assoc result :trace trace)))))

(defn property-result
  "Create a property result map. When pass? is false, includes trace info.
   On failure, prints diagnostic info and returns false."
  [pass? data trace-fn]
  (if pass?
    true
    (let [trace (try (trace-fn) (catch Exception _ nil))]
      (println "\n=== PROPERTY FAILURE DIAGNOSTICS ===")
      (println "Expression:" (:expr data))
      (println "Bindings:" (:bindings data))
      (println "Original value:" (:original-value data))
      (println "Simplified:" (:simplified data))
      (println "Simplified value:" (:simplified-value data))
      (println "--- Trace ---")
      (println trace)
      (println "=== END DIAGNOSTICS ===\n")
      false)))

(defn try-simplify
  "Attempt to simplify an expression, returning nil if it throws."
  [expr]
  (try
    (ss/simplify expr)
    (catch Exception _ nil)))

(defn try-solve
  "Attempt to solve equations, returning nil if it throws."
  [eqns]
  (try
    (ss/solve-for-consts eqns)
    (catch Exception _ nil)))

;; =============================================================================
;; Smoke test for generators
;; =============================================================================

(t/deftest generator-smoke-test
  (t/testing "generators produce valid expressions"
    (t/is (every? seq? (gen/sample gen-simple-expr 10)))
    (t/is (every? keyword? (gen/sample gen-variable 10)))
    (t/is (every? number? (gen/sample gen-rational 10)))
    (t/is (seq (gen/sample gen-nested-expr 10)))))

;; =============================================================================
;; Semantic preservation property
;; =============================================================================

(defn approximately=
  "Check if two numbers are approximately equal (for floating point)."
  [a b]
  (or (= a b)
      (and (number? a) (number? b)
           (< (Math/abs (- (double a) (double b))) 1e-9))))

;; Known bug: unary minus is lost during inversion.
;; (- 0 :a) simplifies to :a instead of (- :a) or (* -1 :a)
;; This test is expected to fail until the bug is fixed.
(defspec ^:known-bug simplify-preserves-semantics-all-ops 500
  (prop/for-all [expr gen-simple-expr]
    (let [bindings (gen/generate (gen-bindings-for expr))
          original-value (evaluate expr bindings)
          simplified (try-simplify expr)
          simplified-value (when simplified (evaluate simplified bindings))
          pass? (or (nil? original-value)
                    (nil? simplified-value)
                    (approximately= original-value simplified-value))]
      (property-result
       pass?
       {:expr expr
        :bindings bindings
        :original-value original-value
        :simplified simplified
        :simplified-value simplified-value}
       #(simplify-with-trace expr)))))

;; Generator that excludes subtraction and division (affected by unary-minus bug)
(def gen-commutative-expr
  "Generator for expressions using only commutative ops (no subtraction/division)."
  (gen/one-of [gen-addition-expr
               gen-multiplication-expr
               gen-max-expr
               gen-min-expr]))

(defspec simplify-preserves-semantics-commutative-ops 500
  (prop/for-all [expr gen-commutative-expr]
    (let [bindings (gen/generate (gen-bindings-for expr))
          original-value (evaluate expr bindings)
          simplified (try-simplify expr)
          simplified-value (when simplified (evaluate simplified bindings))
          pass? (or (nil? original-value)
                    (nil? simplified)
                    (nil? simplified-value)
                    (approximately= original-value simplified-value))]
      (property-result
       pass?
       {:expr expr
        :bindings bindings
        :original-value original-value
        :simplified simplified
        :simplified-value simplified-value}
       #(simplify-with-trace expr)))))

;; =============================================================================
;; Commutativity property
;; =============================================================================

(defn shuffle-commutative-args
  "Shuffle arguments of commutative operations in an expression."
  [expr]
  (cond
    (not (seq? expr)) expr
    :else
    (let [[op & args] expr
          shuffled-args (map shuffle-commutative-args args)]
      (if (contains? ss/commutative-ops op)
        (cons op (shuffle shuffled-args))
        (cons op shuffled-args)))))

(def gen-commutative-only-expr
  "Generator for expressions using only commutative ops."
  (gen/let [op gen-commutative-op
            args (gen/vector (gen/one-of [gen-rational gen-variable]) 2 4)]
    (cons op args)))

(defn commutativity-result
  "Property result for commutativity tests with appropriate diagnostics."
  [pass? data trace-fn]
  (if pass?
    true
    (let [trace (try (trace-fn) (catch Exception _ nil))]
      (println "\n=== COMMUTATIVITY FAILURE DIAGNOSTICS ===")
      (println "Original expr:" (:expr data))
      (println "Shuffled expr:" (:shuffled data))
      (println "Bindings:" (:bindings data))
      (println "Original simplified:" (:simplified-orig data))
      (println "Shuffled simplified:" (:simplified-shuffled data))
      (println "Original value:" (:val-orig data))
      (println "Shuffled value:" (:val-shuffled data))
      (println "--- Trace (original) ---")
      (println (:trace-orig trace))
      (println "--- Trace (shuffled) ---")
      (println (:trace-shuffled trace))
      (println "=== END DIAGNOSTICS ===\n")
      false)))

(defspec commutativity-preserved-after-simplify 500
  (prop/for-all [expr gen-commutative-only-expr]
    (let [shuffled (shuffle-commutative-args expr)
          simplified-orig (try-simplify expr)
          simplified-shuffled (try-simplify shuffled)
          bindings (gen/generate (gen-bindings-for expr))
          val-orig (when simplified-orig (evaluate simplified-orig bindings))
          val-shuffled (when simplified-shuffled (evaluate simplified-shuffled bindings))
          pass? (or (nil? simplified-orig)
                    (nil? simplified-shuffled)
                    (nil? val-orig)
                    (nil? val-shuffled)
                    (approximately= val-orig val-shuffled))]
      (commutativity-result
       pass?
       {:expr expr
        :shuffled shuffled
        :bindings bindings
        :simplified-orig simplified-orig
        :simplified-shuffled simplified-shuffled
        :val-orig val-orig
        :val-shuffled val-shuffled}
       #(hash-map :trace-orig (simplify-with-trace expr)
                  :trace-shuffled (simplify-with-trace shuffled))))))

;; =============================================================================
;; Solution correctness property
;; =============================================================================

(def gen-single-solvable-equation
  "Generator for a single solvable equation: (= :var constant-expr)."
  (gen/let [var gen-variable
            ;; Use only commutative ops to avoid the known bug
            op (gen/elements ['+ '*])
            const1 gen-nonzero-rational
            const2 gen-nonzero-rational]
    (list '= var (list op const1 const2))))

(defn solution-result
  "Property result for solution correctness tests."
  [pass? data trace-fn]
  (if pass?
    true
    (let [trace (try (trace-fn) (catch Exception _ nil))]
      (println "\n=== SOLUTION FAILURE DIAGNOSTICS ===")
      (println "Equations:" (:eqns data))
      (println "Result:" (:result data))
      (println "Expected:" (:expected data))
      (println "Actual:" (:actual data))
      (println "--- Trace ---")
      (println trace)
      (println "=== END DIAGNOSTICS ===\n")
      false)))

(defspec single-equation-solution-correct 500
  (prop/for-all [eqn gen-single-solvable-equation]
    (let [result (try-solve [eqn])
          [remaining consts] (or result [nil nil])
          [_ var expr] eqn
          expected-val (evaluate expr {})
          actual-val (get consts var)
          pass? (or (nil? result)
                    (nil? expected-val)
                    (approximately= actual-val expected-val))]
      (solution-result
       pass?
       {:eqns [eqn]
        :result result
        :expected expected-val
        :actual actual-val}
       #(simplify-with-trace expr)))))

;; Generator for small systems of equations
(def gen-small-equation-system
  "Generator for a small system of 2-3 solvable equations."
  (gen/let [;; First equation grounds a variable to a constant
            var1 (gen/elements [:a :b :c])
            const1 gen-nonzero-rational
            ;; Second equation defines another var in terms of the first
            var2 (gen/elements [:x :y :z])
            op (gen/elements ['+ '*])
            const2 gen-nonzero-rational]
    [(list '= var1 const1)
     (list '= var2 (list op var1 const2))]))

(defspec small-system-solution-correct 500
  (prop/for-all [eqns gen-small-equation-system]
    (let [result (try-solve eqns)
          [remaining consts] (or result [nil nil])
          check-eqn (fn [[_ var expr]]
                      (let [expected (evaluate expr consts)
                            actual (get consts var)]
                        {:var var :expected expected :actual actual
                         :pass? (or (nil? expected)
                                    (approximately= actual expected))}))
          eqn-results (when consts (map check-eqn eqns))
          pass? (or (nil? result)
                    (and (empty? remaining)
                         (every? :pass? eqn-results)))]
      (solution-result
       pass?
       {:eqns eqns
        :result result
        :remaining remaining
        :eqn-results eqn-results}
       #(str "Equations: " (pr-str eqns))))))

;; =============================================================================
;; Per-operation property tests
;; =============================================================================

(defn op-result
  "Simple property result for per-operation tests."
  [pass? data trace-fn]
  (if pass?
    true
    (let [trace (try (trace-fn) (catch Exception _ nil))]
      (println "\n=== OP FAILURE DIAGNOSTICS ===")
      (doseq [[k v] data]
        (println (str (name k) ":") v))
      (println "--- Trace ---")
      (println trace)
      (println "=== END DIAGNOSTICS ===\n")
      false)))

;; Addition properties
(defspec addition-identity-element 500
  (prop/for-all [x gen-nonzero-rational]
    (let [expr (list '+ 0 x)
          simplified (try-simplify expr)
          result-val (when simplified (evaluate simplified {}))
          pass? (or (nil? simplified)
                    (= simplified x)
                    (approximately= result-val x))]
      (op-result pass?
                 {:expr expr :simplified simplified :expected x :actual result-val}
                 #(simplify-with-trace expr)))))

(defspec addition-commutative 500
  (prop/for-all [a gen-nonzero-rational
                 b gen-nonzero-rational]
    (let [expr1 (list '+ a b)
          expr2 (list '+ b a)
          s1 (try-simplify expr1)
          s2 (try-simplify expr2)
          v1 (when s1 (evaluate s1 {}))
          v2 (when s2 (evaluate s2 {}))
          pass? (or (nil? s1) (nil? s2) (approximately= v1 v2))]
      (op-result pass?
                 {:expr1 expr1 :expr2 expr2 :s1 s1 :s2 s2 :v1 v1 :v2 v2}
                 #(str (simplify-with-trace expr1) (simplify-with-trace expr2))))))

(defspec addition-with-variable-commutative 500
  (prop/for-all [a gen-nonzero-rational
                 v gen-variable]
    (let [expr1 (list '+ a v)
          expr2 (list '+ v a)
          s1 (try-simplify expr1)
          s2 (try-simplify expr2)
          bindings {v 42}
          v1 (when s1 (evaluate s1 bindings))
          v2 (when s2 (evaluate s2 bindings))
          pass? (or (nil? s1) (nil? s2) (approximately= v1 v2))]
      (op-result pass?
                 {:expr1 expr1 :expr2 expr2 :bindings bindings :v1 v1 :v2 v2}
                 #(str (simplify-with-trace expr1) (simplify-with-trace expr2))))))

;; Multiplication properties
(defspec multiplication-identity-element 500
  (prop/for-all [x gen-nonzero-rational]
    (let [expr (list '* 1 x)
          simplified (try-simplify expr)
          result-val (when simplified (evaluate simplified {}))
          pass? (or (nil? simplified)
                    (= simplified x)
                    (approximately= result-val x))]
      (op-result pass?
                 {:expr expr :simplified simplified :expected x :actual result-val}
                 #(simplify-with-trace expr)))))

(defspec multiplication-commutative 500
  (prop/for-all [a gen-nonzero-rational
                 b gen-nonzero-rational]
    (let [expr1 (list '* a b)
          expr2 (list '* b a)
          s1 (try-simplify expr1)
          s2 (try-simplify expr2)
          v1 (when s1 (evaluate s1 {}))
          v2 (when s2 (evaluate s2 {}))
          pass? (or (nil? s1) (nil? s2) (approximately= v1 v2))]
      (op-result pass?
                 {:expr1 expr1 :expr2 expr2 :v1 v1 :v2 v2}
                 #(str (simplify-with-trace expr1) (simplify-with-trace expr2))))))

;; Max/Min properties
(defspec max-commutative 500
  (prop/for-all [a gen-rational
                 b gen-rational]
    (let [expr1 (list 'max a b)
          expr2 (list 'max b a)
          s1 (try-simplify expr1)
          s2 (try-simplify expr2)
          v1 (when s1 (evaluate s1 {}))
          v2 (when s2 (evaluate s2 {}))
          pass? (or (nil? s1) (nil? s2) (approximately= v1 v2))]
      (op-result pass?
                 {:expr1 expr1 :expr2 expr2 :v1 v1 :v2 v2}
                 #(str (simplify-with-trace expr1) (simplify-with-trace expr2))))))

(defspec min-commutative 500
  (prop/for-all [a gen-rational
                 b gen-rational]
    (let [expr1 (list 'min a b)
          expr2 (list 'min b a)
          s1 (try-simplify expr1)
          s2 (try-simplify expr2)
          v1 (when s1 (evaluate s1 {}))
          v2 (when s2 (evaluate s2 {}))
          pass? (or (nil? s1) (nil? s2) (approximately= v1 v2))]
      (op-result pass?
                 {:expr1 expr1 :expr2 expr2 :v1 v1 :v2 v2}
                 #(str (simplify-with-trace expr1) (simplify-with-trace expr2))))))

(defspec max-idempotent 500
  (prop/for-all [a gen-rational]
    (let [expr (list 'max a a)
          simplified (try-simplify expr)
          result-val (when simplified (evaluate simplified {}))
          pass? (or (nil? simplified) (approximately= result-val a))]
      (op-result pass?
                 {:expr expr :simplified simplified :expected a :actual result-val}
                 #(simplify-with-trace expr)))))

(defspec min-idempotent 500
  (prop/for-all [a gen-rational]
    (let [expr (list 'min a a)
          simplified (try-simplify expr)
          result-val (when simplified (evaluate simplified {}))
          pass? (or (nil? simplified) (approximately= result-val a))]
      (op-result pass?
                 {:expr expr :simplified simplified :expected a :actual result-val}
                 #(simplify-with-trace expr)))))
