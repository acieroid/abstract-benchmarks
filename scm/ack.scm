;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(letrec ((ack (lambda (m n)
                (if (= m 0)
                    (+ n 1)
                    (if (= n 0)
                        (ack (- m 1) 1)
                        (ack (- m 1) (ack m (- n 1))))))))
  (equal? (ack 3 9) 4093))
