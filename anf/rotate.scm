;; Expected result: "hallo"
(letrec ((rotate (lambda (n x y z) (let ((_16 (= n 0))) (if _16 x (let ((_17 (- n 1))) (let ((_18 (rotate _17 y z x))) _18))))))) (rotate 41 5 #t "hallo"))
