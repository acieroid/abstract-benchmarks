;; Expcted result: 123
(letrec ((count (lambda (n)
                  (if (= n 0)
                      123
                      (count (- n 1))))))
  (count 8))
