;; Expected result: 550
(letrec ((_lp10 (lambda (_i1 _x2) (let ((_a3 (= 0 _i1))) (if _a3 _x2 (letrec ((_lp24 (lambda (_j5 _f6 _y7) (let ((_b8 (= 0 _j5))) (if _b8 (let ((_p10 (- _i1 1))) (_lp10 _p10 _y7)) (let ((_p11 (- _j5 1))) (let ((_p12 (_f6 _y7))) (_lp24 _p11 _f6 _p12)))))))) (_lp24 10 (lambda (_n9) (+ _n9 _i1)) _x2))))))) (_lp10 10 0))
