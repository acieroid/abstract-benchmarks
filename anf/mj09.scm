;; Expected result: 2
(let ((_h0 (lambda (_b1) (let ((_g2 (lambda (_z3) _z3))) (let ((_f4 (lambda (_k5) (if _b1 (_k5 1) (_k5 2))))) (let ((_y6 (_f4 (lambda (_x7) _x7)))) (_g2 _y6))))))) (let ((_x8 (_h0 #t))) (let ((_y9 (_h0 #f))) _y9)))
