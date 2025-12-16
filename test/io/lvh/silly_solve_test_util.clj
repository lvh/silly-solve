(ns io.lvh.silly-solve-test-util
  "Shared test utilities for silly-solve test suites.")

(defn approximately=
  "Check if two numbers are approximately equal (for floating point).
   Returns true if a and b are equal, or if both are numbers and their
   absolute difference is less than 1e-9."
  [a b]
  (or (= a b)
      (and (number? a) (number? b)
           (< (Math/abs (- (double a) (double b))) 1e-9))))
