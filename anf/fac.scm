;; Expected result: 40320
(letrec ((fac (lambda (n) (let ((_16 (= n 0))) (if _16 1 (let ((_19 (- n 1))) (let ((_17 _19)) (let ((_20 (fac _17))) (let ((_18 _20)) (let ((_21 (* n _18))) _21)))))))))) (fac 8))
