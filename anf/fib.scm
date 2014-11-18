;; Expected result: 21
(letrec ((fib (lambda (n) (let ((_16 (< n 2))) (if _16 n (let ((_21 (- n 1))) (let ((_17 _21)) (let ((_22 (fib _17))) (let ((_18 _22)) (let ((_23 (- n 2))) (let ((_19 _23)) (let ((_24 (fib _19))) (let ((_20 _24)) (let ((_25 (+ _18 _20))) _25)))))))))))))) (fib 8))
