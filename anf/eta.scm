;; Expected result: #f
(let ((_do-something0 (lambda () 10))) (let ((_id1 (lambda (_y2) (let ((_tmp13 (_do-something0))) _y2)))) (let ((_p7 (_id1 (lambda (_a5) _a5)))) (let ((_tmp24 (_p7 #t))) (let ((_p8 (_id1 (lambda (_b6) _b6)))) (_p8 #f))))))
