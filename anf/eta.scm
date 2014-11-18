;; Expected result: #f
(letrec ((do-something (lambda () 10))) (letrec ((id (lambda (y) (let ((_16 (do-something))) (letrec ((tmp1 _16)) y))))) (let ((_17 (id (lambda (a) a)))) (let ((_18 (_17 #t))) (letrec ((tmp2 _18)) (let ((_19 (id (lambda (b) b)))) (let ((_20 (_19 #f))) _20)))))))
