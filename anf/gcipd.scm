;; Expected result: 36
(let ((_id0 (lambda (_x1) _x1))) (letrec ((_f2 (lambda (_n3) (let ((_p6 (<= _n3 1))) (if _p6 1 (let ((_p7 (- _n3 1))) (let ((_p8 (_f2 _p7))) (* _n3 _p8)))))))) (letrec ((_g4 (lambda (_n5) (let ((_p9 (<= _n5 1))) (if _p9 1 (let ((_p10 (* _n5 _n5))) (let ((_p11 (- _n5 1))) (let ((_p12 (_g4 _p11))) (+ _p10 _p12))))))))) (let ((_p13 (_id0 _f2))) (let ((_p14 (_p13 3))) (let ((_p15 (_id0 _g4))) (let ((_p16 (_p15 4))) (+ _p14 _p16))))))))
