;; Expected result: 5
;; From Multi-core Parallelization of Abstracted Abstract Machines, Andersen and Might, 2013
(letrec ((even? (lambda (n)
                  (if (= n 0)
                      #t
                      (if (= n 1)
                          #f
                          (even? (- n 2)))))))
  (letrec ((div2* (lambda (n s)
                    (if (= (* 2 n) s)
                        n
                        (if (= (+ (* 2 n) 1) s)
                            n
                            (div2* (- n 1) s))))))
    (letrec ((div2 (lambda (n)
                     (div2* n n))))
      (letrec ((hailstone* (lambda (n count)
                             (if (= n 1)
                                 count
                                 (if (even? n)
                                     (hailstone* (div2 n) (+ count 1))
                                     (hailstone* (+ (* 3 n) 1)
                                                 (+ count 1)))))))
        (letrec ((hailstone (lambda (n) (hailstone* n 0))))
          (hailstone 5))))))
