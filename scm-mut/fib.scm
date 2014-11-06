;; Expected result: 21
(let ((fib (lambda (n)
             (let ((a 0)
                   (b 1))
               (letrec ((loop (lambda (x)
                                (if (= x n)
                                    a
                                    (let ((olda a))
                                      (begin
                                        (set! a b)
                                        (set! b (+ olda b))
                                        (loop (+ x 1))))))))
                 (loop 0))))))
  (fib 8))
