;; Expected result: 21
(let ((fib (lambda (n) (let ((a 0)) (let ((b 1)) (letrec ((loop (lambda (x) (let ((_16 (= x n))) (if _16 a (let ((olda a)) (let ((_17 (set! a b))) (let ((_20 (+ olda b))) (let ((_19 _20)) (let ((_18 (set! b _19))) (let ((_21 (+ x 1))) (let ((_22 (loop _21))) _22)))))))))))) (loop 0))))))) (fib 8))
