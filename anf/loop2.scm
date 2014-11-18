;; Expected result: 550
(letrec ((lp1 (lambda (i x) (let ((_16 (= 0 i))) (letrec ((a _16)) (if a x (letrec ((lp2 (lambda (j f y) (let ((_17 (= 0 j))) (letrec ((b _17)) (if b (let ((_18 (- i 1))) (let ((_19 (lp1 _18 y))) _19)) (let ((_20 (- j 1))) (let ((_21 (f y))) (let ((_22 (lp2 _20 f _21))) _22))))))))) (lp2 10 (lambda (n) (+ n i)) x)))))))) (lp1 10 0))
