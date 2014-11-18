;; Expected result: #f
((lambda (f1) (let ((_16 (f1 #t))) (letrec ((a _16)) (f1 #f)))) (lambda (x1) ((lambda (f2) (let ((_17 (f2 #t))) (letrec ((b _17)) (f2 #f)))) (lambda (x2) ((lambda (f3) (let ((_18 (f3 #t))) (letrec ((c _18)) (f3 #f)))) (lambda (x3) ((lambda (z) (z x1 x2 x3)) (lambda (y1 y2 y3) y1))))))))
