;; Expected result: #f
((lambda (f1) (let ((_16 (f1 #t))) (letrec ((a _16)) (f1 #f)))) (lambda (x1) ((lambda (f2) (let ((_17 (f2 #t))) (letrec ((b _17)) (let ((_18 (f2 #f))) (letrec ((c _18)) (f2 #t)))))) (lambda (x2) ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))
