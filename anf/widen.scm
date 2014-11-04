(let ((g (lambda () 1)))
  (letrec ((f (lambda (n)
                (let ((test (= n 0)))
                  (if test
                      0
                      (let ((decn (- n 1)))
                        (let ((arg1 (f decn)))
                          (let ((arg2 (g)))
                            (+ arg1 arg2)))))))))
    (f 10)))
