;; Expected result: 123
(letrec ((count (lambda (n) (let ((_16 (= n 0))) (if _16 123 (let ((_17 (- n 1))) (let ((_18 (count _17))) _18))))))) (count 8))
