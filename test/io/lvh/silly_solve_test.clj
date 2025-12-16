(ns io.lvh.silly-solve-test
  (:require
   [clojure.test :as t]
   [io.lvh.silly-solve :as ss]
   [meander.strategy.epsilon :as r]))

(t/deftest invert-tests
  (t/is (= '(+ 1 (- 2) (- 3) (- 4) (- 5))
           (#'ss/invert '- [1 2 3 4 5])))
  (t/is (= '(* 1 (/ 2) (/ 3) (/ 4) (/ 5))
           (#'ss/invert '/ [1 2 3 4 5]))))

(t/deftest find-consts-tests
  ;; Note: all the tests assume the const comes first in the equation, because
  ;; it's not find-consts' job to simplify.
  (t/are [eqns res] (= res (#'ss/find-consts eqns))
    []
    {}

    '[(= x (/ y 2))]
    {}

    '[(= 1 x)]
    '{x 1}

    '[(= 1 x) (= 2 y)]
    '{x 1 y 2}

    '[(= 1 x)
      (= x (/ y 2))]
    '{x 1}

    '[(= 1 :x)]
    '{:x 1}

    '[(= 1 :x)]
    '{:x 1}

    '[(= 1 :x :y)]
    '{:x 1 :y 1}

    '[(= 1 :x :y (+ :some :complex :equation))]
    '{:x 1 :y 1}))

(t/deftest propagate-consts-tests
  (t/are [eqns consts res] (= res (#'ss/propagate-consts eqns consts))
    '[]
    '{}
    '[]

    '[(= 1 x) (= y 2)]
    '{}
    '[(= 1 x) (= y 2)]

    ;; Note: we expect consts to appear in the front but that's not
    ;; propagate-consts' job
    '[(= x y)]
    '{y 1}
    '[(= x 1)]

    ;; Constants get resolved even in deeply nested contexts
    '[(= p (+ x y))
      (= q (+ x z))]
    '{x 1 y 2 z 3}
    '[(= p (+ 1 2))
      (= q (+ 1 3))]

    ;; Expressions that _don't_ look like (= ?sym ?expr) are untouched, so that
    ;; other, smarter solvers might be able to make progress instead.
    '[(= (+ p q) (r s))
      (= x y)]
    '{y 1}
    '[(= (+ p q) (r s))
      (= x 1)]

    ;; However, we can find constants inside such more complex expressions
    '[(= (+ x z) (+ x y))
      (= q (+ x z))]
    '{z 1}
    '[(= (+ x 1) (+ x y))
      (= q (+ x 1))]))

(def traced-simplify (r/trace ss/simplify))

(t/deftest simplify-tests
  (t/testing "trivial unary reductions"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ 1) 1
      '(- 1) -1
      '(* 1) 1))

  (t/testing "trivial unary reductions, but with variables"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ :x) :x
      '(* :x) :x))

  (t/testing "neutral elements"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ 0 x) 'x
      '(* 1 x) 'x))

  (t/testing "neutral elements in sums with >2 elements"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ 0 0 0 x) 'x
      '(+ x 0 0 0) 'x
      '(+ 0 x 0 0) 'x))

  (t/testing "neutral elements in products with >2 elements"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(* 1 1 1 x) 'x
      '(* x 1 1 1) 'x
      '(* 1 x 1 1) 'x))

  (comment
    ;; We don't currently get rid of the neutral element with multiple variables,
    ;; though we do still coalesce constants:
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ 0 x y) '(+ x y)
      '(* 1 x y) '(* x y)))

  (t/testing "addition with constants"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(+ 1 1) 2
      '(+ 1 1 1) 3
      '(+ 1 1 1 1) 4))

  (t/testing "addition with constants and variables"
    (t/are [eqn] (= '(+ 4 x) (traced-simplify eqn))
      '(+ 1 1 1 1 x)
      '(+ 1 1 x 1 1)
      '(+ x 1 1 1 1)))

  (t/testing "subtraction with constants"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(- 10 1) 9
      '(- 10 1 2 3) 4))

  (t/testing "subtraction with constants and variables"
    ;; (- 1 1 1 1 x) = 1-1-1-1-x = -2-x = (+ -2 (- x))
    (t/are [eqn] (= '(+ -2 (- x)) (traced-simplify eqn))
      '(- 1 1 1 1 x)
      '(- 1 1 1 x 1)
      '(- 1 1 x 1 1))
    ;; (- x 1 1 1 1) = x-1-1-1-1 = x-4 = (+ -4 x)
    (t/is (= '(+ -4 x) (traced-simplify '(- x 1 1 1 1))))))

(t/deftest keywords-work-too-tests
  (let [sym-system '[(= x 3)
                     (= y (* 2 x))
                     (= z (+ x y))]
        kw-system '[(= :x 3)
                    (= :y (* 2 :x))
                    (= :z (+ :x :y))]
        get-consts (fn [system]
                     (let [[_ consts] (ss/solve-for-consts system)]
                       (update-keys consts name)))]
    (t/is
     (=
      (get-consts sym-system)
      (get-consts kw-system)
      {"x" 3 "y" 6 "z" 9}))))

(t/deftest min-max-tests
  (t/is
   (= [[] {'x 5 'y 1}]
      (ss/solve-for-consts
       ['(= x (max 1 2 3 4 5))
        '(= y (min 1 2 3 4 5))])))

  (t/is
   (= [[] {'x 5 'y 1 'p 5 'q 1}]
      (ss/solve-for-consts
       ['(= x (max 1 2 3 4 5))
        '(= y (min 1 2 3 4 5))
        '(= p (max x y 1 2 3))
        '(= q (min x y 4 5 6))]))))

(t/deftest equality-const-to-front
  (t/are [in out] (= out (#'ss/equality-const-to-front in))
    [10 :a]
    '(= 10 :a)

    [:a 10]
    '(= 10 :a)

    [10 :a 10 :b]
    '(= 10 :a :b)

    [:a 10 :b 10 :c]
    '(= 10 :a :b :c)

    [:a :b 10 10 :c]
    '(= 10 :a :b :c))

  (t/is
   (=
    (ss/solve-for-consts '[(= :b 10)])
    (ss/solve-for-consts '[(= 10 :b)])
    [[] {:b 10}])))

(t/deftest deal-with-multi-valued-equality-tests
  (t/are [in-exprs] (= [[] {:a 10 :b 10 :c 10}] (ss/solve-for-consts in-exprs))
    '[(= :a :b :c 10)]
    '[(= :a 10 :b :c)]
    '[(=  10 :a :b :c)]
    '[(= :a 10) (= :a :b :c)]))

(t/deftest nested-multiplication-in-subtraction-bug-test
  ;; Bug: when a variable that equals 1 is substituted into (* 25 :both),
  ;; the result is incorrectly computed as 50 instead of 25.
  ;; This causes :r-both to be 50 instead of 15.
  (t/testing "multiplication by 1 should not double the value"
    (t/is (= [[] {:none 0
                  :one 0
                  :both 1
                  :r-example 15
                  :r-none 25
                  :r-both 15}]
             (ss/solve-for-consts
              '[(= :none (* 0 0))
                (= :one (* 1 0))
                (= :both (* 1 1))
                (= :r-example (max (- 25 (* 25 1)) (/ 30 2)))
                (= :r-none (max (- 25 (* 25 :none)) (/ 30 2)))
                (= :r-both (max (- 25 (* 25 :both)) (/ 30 2)))]))))

  (t/testing "simpler reproduction: (* 25 1) should equal 25, not 50"
    (t/is (= 25 (ss/simplify '(* 25 1))))))
