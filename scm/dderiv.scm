(let ((lookup (lambda (key table)
                (letrec ((loop (lambda (x)
                                 (if (null? x)
                                     #f
                                     (let ((pair (car x)))
                                       (if (eq? (car pair) key)
                                           pair
                                           (loop (cdr x))))))))
                  (loop table)))))
  (let ((properties '()))
    (let ((get (lambda (key1 key2)
                 (let ((x (lookup key1 properties)))
                   (if x
                       (let ((y (lookup key2 (cdr x))))
                         (if y
                             (cdr y)
                             #f))
                       #f)))))
      (let ((put (lambda (key1 key2 val)
                   (let ((x (lookup key1 properties)))
                     (if x
                         (let ((y (lookup key2 (cdr x))))
                           (if y
                               (set-cdr! y val)
                               (set-cdr! x (cons (cons key2 val) (cdr x)))))
                         (set! properties
                               (cons (cons key1 (cons (cons key2 val) '())) properties)))))))
        (let ((dderiv (lambda (a)
                        (if (not (pair? a))
                            (if (eq? a 'x) 1 0)
                            (let ((f (get (car a) 'dderiv)))
                              (if f
                                  (f a)
                                  (error "No derivation method available")))))))
          (let ((my+dderiv (lambda (a)
                             (cons '+
                                   (map dderiv (cdr a))))))

            (let ((my-dderiv (lambda (a)
                               (cons '-
                                     (map dderiv (cdr a))))))

              (let ((*dderiv (lambda (a)
                               (cons '*
                                     (cons a
                                           (cons (cons '+
                                                       (map (lambda (a) (cons '/ (cons (dderiv a) (cons a '())))) (cdr a)))
                                                 '()))))))

                (let ((/dderiv (lambda (a)
                                 (cons '-
                                       (cons (cons '/
                                                   (cons (dderiv (cadr a))
                                                         (cons (caddr a) '())))
                                             (cons (cons '/
                                                         (cons (cadr a)
                                                               (cons (cons '*
                                                                           (cons (caddr a)
                                                                                 (cons (caddr a)
                                                                                       (cons (dderiv (caddr a))
                                                                                             '()))))
                                                                     '())))
                                                   '()))))))
                  (put '+ 'dderiv my+dderiv)
                  (put '- 'dderiv my-dderiv)
                  (put '* 'dderiv *dderiv)
                  (put '/ 'dderiv /dderiv)
                  (let ((arg '(+ (* 3 x x) (* a x x) (* b x) 5))
                        (result '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                                    (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                                    (* (* b x) (+ (/ 0 b) (/ 1 x)))
                                    0)))
                    (equal? (dderiv arg) result)))))))))))
