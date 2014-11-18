;; Expected result: 6
(letrec ((tak (lambda (x y z k) (let ((_16 (< y x))) (let ((_17 (not _16))) (if _17 (k z) (let ((_22 (- x 1))) (let ((_23 (tak _22 y z (lambda (v1) (let ((_20 (- y 1))) (let ((_21 (tak _20 z x (lambda (v2) (let ((_18 (- z 1))) (let ((_19 (tak _18 x y (lambda (v3) (tak v1 v2 v3 k))))) _19)))))) _21)))))) _23)))))))) (tak 20 10 5 (lambda (a) a)))
