;; Expected result: 30
;; Taken from the SICP
(define message-withdraw 0)
(define message-deposit 1)
(define (make-account balance)
  (letrec ((withdraw (lambda (amount)
                       (if (>= balance amount)
                           (begin (set! balance (- balance amount))
                                  balance)
                           -1)))
           (deposit (lambda (amount)
                      (begin (set! balance (+ balance amount))
                             balance)))
           (dispatch (lambda (m)
                       (if (= m message-withdraw)
                           withdraw
                           (if (= m message-deposit)
                               deposit
                               -2)))))
    dispatch))

(let ((account (make-account 100)))
  (begin
    ((account message-withdraw) 50) ; 50
    ((account message-withdraw) 60) ; insufficient funds, still 50 remaining
    ((account message-deposit) 40)
    ((account message-withdraw) 60) 30))
