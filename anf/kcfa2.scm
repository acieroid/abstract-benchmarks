;; Expected result: #f
(let ((_f10 (lambda (_x12) (let ((_f23 (lambda (_x26) (let ((_z7 (lambda (_y18 _y29) _y18))) (_z7 _x12 _x26))))) (let ((_b4 (_f23 #t))) (let ((_c5 (_f23 #f))) (_f23 #t))))))) (let ((_a1 (_f10 #t))) (_f10 #f)))
