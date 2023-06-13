(ns io.lvh.silly-solve-test
  (:require [clojure.test :as t]
            [io.lvh.silly-solve :as ss]
            [meander.strategy.epsilon :as r]))

(t/deftest invert-tests
  (t/is (= '(+ 1 (- 2) (- 3) (- 4) (- 5))
           (#'ss/invert '- [1 2 3 4 5])))
  (t/is (= '(* 1 (/ 2) (/ 3) (/ 4) (/ 5))
           (#'ss/invert '/ [1 2 3 4 5]))))

(t/deftest find-const-tests
  (t/are [eqns res] (= res (#'ss/find-consts eqns))
    [] {}

    '[(= x (/ y 2))]
    {}

    '[(= x 1)]
    '{x 1}

    '[(= x 1) (= y 2)]
    '{x 1 y 2}

    '[(= x 1)
      (= x (/ y 2))]
    '{x 1}


    '[(= :x 1)]
    '{:x 1}))

(t/deftest propagate-consts-tests
  (t/are [eqns consts res] (= res (#'ss/propagate-consts eqns consts))
    '[]
    '{}
    '[]

    '[(= x 1) (= y 2)]
    '{}
    '[(= x 1) (= y 2)]

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
      '(+ 0 x 0 0) 'x

      '(+ 0 x y) '(+ x y)))

  (t/testing "neutral elements in products with >2 elements"
    (t/are [eqn res] (= res (traced-simplify eqn))
      '(* 1 1 1 x) 'x
      '(* x 1 1 1) 'x
      '(* 1 x 1 1) 'x

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
    (t/are [eqn] (= '(+ -2 x) (traced-simplify eqn))
      '(- 1 1 1 1 x)
      '(- 1 1 1 x 1)
      '(- 1 1 x 1 1))
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
