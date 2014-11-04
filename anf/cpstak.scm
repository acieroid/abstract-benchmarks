;; Expected result: 6
(letrec ((tak (lambda (x y z k)
                (let ((lt (< y x)))
                  (let ((test (not lt)))
                    (if test
                        (k z)
                        (let ((decx (- x 1)))
                          (tak decx
                               y
                               z
                               (lambda (v1)
                                 (let ((decy (- y 1)))
                                   (tak decy
                                        z
                                        x
                                        (lambda (v2)
                                          (let ((decz (- z 1)))
                                            (tak decz
                                                 x
                                                 y
                                                 (lambda (v3)
                                                   (tak v1 v2 v3 k))))))))))))))))
  (tak 20 10 5 (lambda (a) a)))
