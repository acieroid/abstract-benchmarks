;; Expected result: 40320
(letrec ((fac (lambda (n)
                (if (= n 0)
                    1
                    (* n (fac (- n 1)))))))
  (fac 8))
