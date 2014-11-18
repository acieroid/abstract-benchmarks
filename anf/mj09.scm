;; Expected result: 2
(letrec ((h (lambda (b) (letrec ((g (lambda (z) z))) (letrec ((f (lambda (k) (if b (k 1) (k 2))))) (let ((_16 (f (lambda (x) x)))) (letrec ((y _16)) (g y)))))))) (let ((_17 (h #t))) (letrec ((x _17)) (let ((_18 (h #f))) (letrec ((y _18)) y)))))
