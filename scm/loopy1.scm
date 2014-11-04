;; Expected result: bottom
(letrec ((f (lambda () (f)))) (f))                  (
