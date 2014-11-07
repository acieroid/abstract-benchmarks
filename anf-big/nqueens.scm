;; Expected result: 92
(letrec ((one-to
          (lambda (n)
            (letrec ((loop
                      (lambda (i l)
                        (let ((_33 (= i 0)))
                          (if _33
                              l
                              (let ((_34 (- i 1)))
                                (let ((_35 (cons i l)))
                                  (let ((_36 (loop _34 _35))) _36))))))))
              (let ((_37 '())) (let ((_38 (loop n _37))) _38))))))
  (letrec ((try-it
            (lambda (x y z)
              (let ((_39 (null? x)))
                (if _39
                    (let ((_40 (null? y))) (if _40 1 0))
                    (let ((_41 (car x)))
                      (let ((_42 (ok? _41 1 z)))
                        (let ((_43 (cdr x)))
                          (let ((_44 (car x)))
                            (let ((_45 (cons _44 y)))
                              (let ((_46 (try-it _43 _45 z)))
                                (let ((_47
                                       (+
                                        (if _42
                                            (try-it
                                             (append (cdr x) y)
                                             '()
                                             (cons (car x) z))
                                            0)
                                        _46)))
                                  _47))))))))))))
    (letrec ((ok?
              (lambda (row dist placed)
                (let ((_48 (null? placed)))
                  (if _48
                      #t
                      (let ((_49 (car placed)))
                        (let ((_50 (+ row dist)))
                          (let ((_51 (= _49 _50)))
                            (let ((_52 (not _51)))
                              (let ((_53 (car placed)))
                                (let ((_54 (- row dist)))
                                  (let ((_55 (= _53 _54)))
                                    (let ((_56 (not _55)))
                                      (let ((_57 (+ dist 1)))
                                        (let ((_58 (cdr placed)))
                                          (let ((_59 (ok? row _57 _58)))
                                            (let ((_60 (and _52 _56 _59)))
                                              _60)))))))))))))))))
      (letrec ((nqueens
                (lambda (n)
                  (let ((_61 (one-to n)))
                    (let ((_62 '()))
                      (let ((_63 '()))
                        (let ((_64 (try-it _61 _62 _63))) _64)))))))
        (nqueens 8)))))
