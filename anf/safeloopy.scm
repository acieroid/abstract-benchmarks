;; Expected result: 123
(letrec ((count (lambda (n)
                  (let ((t (= n 0)))
                    (if t
                        123
                        (let ((u (- n 1)))
                          (let ((v (count u)))
                            v)))))))
  (count 8))
