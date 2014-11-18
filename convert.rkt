#lang racket
;; Converts Scheme expressions to ANF by preserving most of the semantics.
;; Some cases of incompatibilities are:
;;   - let becomes equivalent to let*
;;   - no support for mutual recursion
;;   - no support (yet) for case
;;   - doesn't seem to work with internal defines

(require racket/match)
(require rackunit)

;; '(1 2 3) -> '((1 2) . 3
(define (split-last l)
  (if (null? (cdr l))
      `(() . ,(car l))
      (match (split-last (cdr l))
        [(cons rest last) (cons (cons (car l) rest) last)])))

(check-equal? (split-last '(1 2 3)) '((1 2) . 3))

(define (atom? exp)
  (not (pair? exp)))

(check-true (atom? 'foo))
(check-false (atom? '(foo)))

;; true for expressions that don't need to extract definitions to become atomic
(define (extract-free? exp)
  (or (atom? exp) (equal? (car exp) 'lambda) (equal? (car exp) 'quote)
      ;; (equal? (car exp) 'let) (equal? (car exp) 'letrec) (equal? (car exp) 'let*)
      ))

(check-true (extract-free? 'foo))
(check-true (extract-free? '(lambda (x) x)))
(check-true (extract-free? '(lambda (x) (+ (* x 2))))) ;; should be ANFized inside the lambda
(check-true (extract-free? '(let ((x 1)) (+ (* x 2))))) ;; same for lets
(check-false (extract-free? '(+ (* x 2))))

;; insert-in (let ((x 0)) __) x -> (let ((x 0)) x)
(define (insert-in e1 e2)
  (cond
   ((equal? e1 '__) e2)
   ((and (pair? e1) (or (equal? (car e1) 'let) (equal? (car e1) 'letrec) (equal? (car e1) 'let*)))
    `(,(car e1) ,(cadr e1) ,(insert-in (caddr e1) e2)))
   (else e1)))

(check-equal? (insert-in '(let ((x 0)) __) 'x) '(let ((x 0)) x))
(check-equal? (insert-in '(let ((x 0)) x) 'y) '(let ((x 0)) x))
(check-equal? (insert-in '(let ((x (* 2 3))) (let ((y (+ x 1))) __)) 'y)
              '(let ((x (* 2 3))) (let ((y (+ x 1))) y)))
(check-equal? (insert-in (insert-in '(let ((x 0)) __) '(let ((y (+ x 1))) __)) 'y)
              '(let ((x 0)) (let ((y (+ x 1))) y)))

(define id 0)
(define (newid)
  (set! id (+ id 1))
  (string->symbol (string-append "_" (number->string id))))

(set! id 0)
(check-equal? (newid) '_1)
(check-equal? (newid) '_2)
(define-syntax-rule (test x y)
  (let ()
    (set! id 0)
    (check-equal? x y)))

(define (extract-defs exp)
  (cond
   ((extract-free? exp) (cons '__ exp))
   ((equal? (car exp) 'set!)
    (match (extract-defs (caddr exp))
      [(cons defs var)
       (cons defs `(set! ,(cadr exp) ,var))]))
   ((equal? (car exp) 'if)
    (match (extract-defs (cadr exp))
      [(cons defs var)
       (cons defs `(if ,var ,(caddr exp) ,(cadddr exp)))]))
   ((or (equal? (car exp) 'let) (equal? (car exp) 'letrec))
    (let ((binding (caadr exp)))
      (match (extract-defs (cadr binding))
        [(cons defs-binding var-binding)
         (match (extract-defs (caddr exp))
           [(cons defs var)
            (cons
             (insert-in defs-binding
                        (insert-in `(,(car exp) ((,(car binding) ,var-binding)) __) defs)) var)])])))
   (else
    ;; Function call
    (match (foldl (lambda (e acc)
                   (match (extract-defs e)
                     [(cons defs var)
                      (cons (insert-in (car acc) defs)
                            (cons var (cdr acc)))]))
                  (cons '__ '())
                 exp)
      [(cons defs rev-vars)
       (let ((var (newid)))
         (cons (insert-in defs `(let ((,var ,(reverse rev-vars))) __))
               var))]))))

(test (extract-defs '(set! x (+ x 1))) '((let ((_1 (+ x 1))) __) . (set! x _1)))
(test (extract-defs '((id f) (id x))) '((let ((_1 (id f))) (let ((_2 (id x))) (let ((_3 (_1 _2))) __))) . _3))
(test (extract-defs '(let ((x 1)) x)) '((let ((x 1)) __) . x))
;; TODO
(test (extract-defs '(let ((x (set! x 1))) y)) '((let ((x (set! x 1))) __) . y))
(test (extract-defs '(if (> x 1) a b)) '((let ((_1 (> x 1))) __) . (if _1 a b)))
(test (extract-defs '(f x (+ y 1))) '((let ((_1 (+ y 1))) (let ((_2 (f x _1))) __)) . _2))
(test (extract-defs '(let ((x 1)) x)) '((let ((x 1)) __) . x))
(test (extract-defs '(f (let ((x 1)) x))) '((let ((x 1)) (let ((_1 (f x))) __)) . _1))
;; TODO: fails
(test (extract-defs '(let ((x (set! x (+ x 1)))) y)) '((let ((_1 (+ x 1))) (let ((x (set! x _1))) __)) . y))

;; Only works with a bunch of defines followed by one main expression
;; '((define x 1) foo) -> (letrec ((x 1)) foo)
(define (remove-defines exp)
  (if (and (pair? exp) (pair? (car exp)) (equal? (caar exp) 'define))
      (let ((def (car exp)))
        (if (pair? (cadr def))
            (insert-in `(letrec ((,(caadr def) (lambda ,(cdadr def) ,(caddr def)))) __) (remove-defines (cdr exp)))
            (insert-in `(let ((,(cadr def) ,(caddr def))) __) (remove-defines (cdr exp)))))
      (if (= (length exp) 1)
          (car exp)
          `(begin ,@exp))))

(test (remove-defines '((define x 1) x)) '(let ((x 1)) x))
(test (remove-defines '((define (id x) x) (id 3))) '(letrec ((id (lambda (x) x))) (id 3)))
(test (remove-defines '((define a 1) (define b 2) (define (c x) 3) (+ a b (c 1))))
      '(let ((a 1)) (let ((b 2)) (letrec ((c (lambda (x) 3))) (+ a b (c 1))))))

(define (simplify-quote exp)
  (if (and (pair? exp) (equal? (car exp) 'quote))
      (letrec ((loop (lambda (e)
                       (cond
                        ((null? e) '(quote ()))
                        ((pair? e) `(cons ,(loop (car e)) ,(loop (cdr e))))
                        (else `(quote ,e))))))
        (loop (cadr exp)))
      exp))

(test (simplify-quote ''foo) ''foo)
(test (simplify-quote ''(foo bar (baz)))
      '(cons 'foo (cons 'bar (cons (cons 'baz '()) '()))))
(test (eval (simplify-quote ''(foo bar (baz)))) '(foo bar (baz)))
(test (eval (simplify-quote ''((foo) (bar (baz (qux))) (baz (bar) (qux quux)))))
      '((foo) (bar (baz (qux))) (baz (bar) (qux quux))))
(test (simplify-quote '(if (> x 1) a b)) '(if (> x 1) a b))

(define (make-begin exps)
  (if (= (length exps) 1)
      (car exps)
      `(begin ,@exps)))

(test (make-begin '(foo)) 'foo)
(test (make-begin '(foo bar)) '(begin foo bar))

(define (remove-cond exp)
  (if (and (pair? exp) (equal? (car exp) 'cond))
      (if (equal? (caadr exp) 'else)
          (make-begin (cdadr exp))
          `(if ,(caadr exp)
               ,(make-begin (cdadr exp))
               ,(remove-cond
                 (if (pair? (cddr exp))
                     `(cond ,@(cddr exp))
                     (begin
                       (display "cond with no fallthrough branch: ") (display exp) (newline)
                       #f)))))
      exp))

(test (remove-cond '(cond ((= x 1) a) ((= x 2) b) ((= x 3) c) (else d)))
      '(if (= x 1) a (if (= x 2) b (if (= x 3) c d))))
(test (remove-cond '(cond ((= x 1) a b c) (else d e f)))
      '(if (= x 1) (begin a b c) (begin d e f)))

;; Won't preverve semantics in the presence of mutual recursion. To use
;; mutual-recursion, the input program has to be modified to explicitely do a
;; let/define followed by a set!
(define (simplify-lets exp)
  (if (pair? exp)
      (cond
       ((or (equal? (car exp) 'let) (equal? (car exp) 'letrec))
        (let ((sym (car exp)))
          (insert-in (foldl (lambda (binding acc)
                              (insert-in acc `(,sym ((,(car binding) ,(simplify-lets (cadr binding)))) __)))
                            '__
                            (cadr exp))
                     (make-begin (cddr exp)))))
       ((equal? (car exp) 'let*)
        (simplify-lets (cons 'let (cdr exp))))
       (else
        exp))
      exp))

(test (simplify-lets '(let ((x 1) (y 2)) body)) '(let ((x 1)) (let ((y 2)) body)))
(test (simplify-lets '(let ((x 1) (y 2)) a b)) '(let ((x 1)) (let ((y 2)) (begin a b))))
(test (simplify-lets '(let* ((x 1) (y 2)) body)) '(let ((x 1)) (let ((y 2)) body))) ; get rid of let*
(test (simplify-lets '(letrec ((x 1) (y 2)) body)) '(letrec ((x 1)) (letrec ((y 2)) body)))
;; TODO: fails for mutual recursion
(test (simplify-lets '(letrec ((x (lambda () (y))) (y (lambda () (x)))) body))
      '(let ((x #f)) (let ((y (lambda () (x)))) (begin (set! x (lambda () (y))) body))))

(define (to-anf exp)
  (cond
   ;; v
   ((atom? exp)
    exp)
   ;; lam
   ((equal? (car exp) 'lambda)
    `(lambda ,(cadr exp) ,(to-anf (make-begin (cddr exp)))))
   ;; (set! v e) -> (let ... (set! v ae))
   ((equal? (car exp) 'set!)
    (match (extract-defs (caddr exp))
      [(cons defs var) (insert-in defs `(set! ,(cadr exp) ,var))]))
   ;; (if e e e) -> (let ... (if ae ae ae))
   ((equal? (car exp) 'if)
    (match (extract-defs (cadr exp))
      [(cons defs-cond var-cond)
       (insert-in defs-cond `(if ,var-cond
                              ,(to-anf (caddr exp))
                              ,(to-anf (cadddr exp))))]))
   ;; (begin e1 e2 ...) -> (let ((_ e1)) (let ((_ e2)) ...))
   ((equal? (car exp) 'begin)
    (to-anf
     (match (split-last (cdr exp))
       [(cons exps last)
        (insert-in (foldl (lambda (e acc)
                            (insert-in acc `(let ((,(newid) ,(to-anf e))) __)))
                          '__
                          exps)
                   last)])))
   ;; (let ((id e)) e) -> (let ... (let ((id ce)) e))
   ((or (equal? (car exp) 'let) (equal? (car exp) 'letrec) (equal? (car exp) 'let*))
    (let ((exp (simplify-lets exp)))
      (let ((kwd (car exp))
            (boundvar (caaadr exp))
            (subexp (cadr (caadr exp)))
            (body (caddr exp)))
        (match (extract-defs subexp)
          [(cons defs var)(insert-in defs `(,kwd ((,boundvar ,(to-anf var)))
                                                 ,(to-anf body)))]))))
   ((equal? (car exp) 'cond)
    (to-anf (remove-cond exp)))
   ((and (equal? (car exp) 'quote) (pair? (cadr exp)))
    (to-anf (simplify-quote exp)))
   ;; (f e...) -> (let ... (f ae...))
   ((pair? exp)
    (if (foldl (lambda (e acc)
                 (and acc (extract-free? e)))
               #t
               exp)
        (map to-anf exp)
        (match (extract-defs (map to-anf exp))
          [(cons defs var) (insert-in defs var)])))
   (else (display "Can't deal with expression: ") (display exp))))

(test (to-anf 'x) 'x)
(test (to-anf '(lambda (x) x)) '(lambda (x) x))
;; TODO: could be improved
(test (to-anf '(lambda (x) (+ (* x x) x))) '(lambda (x) (let ((_1 (* x x))) (let ((_2 (+ _1 x))) _2))))
(test (to-anf '(lambda (x) x x)) '(lambda (x) (let ((_1 x)) x)))
(test (to-anf '(set! x 1)) '(set! x 1))
(test (to-anf '(set! x (+ x 1))) '(let ((_1 (+ x 1))) (set! x _1)))
(test (to-anf '(set! x (+ (* x 2) 1))) '(let ((_1 (* x 2))) (let ((_2 (+ _1 1))) (set! x _2))))
(test (to-anf '(if a b c)) '(if a b c))
(test (to-anf '(if (= x 0) 1 2)) '(let ((_1 (= x 0))) (if _1 1 2)))
(test (to-anf '(if (= (- x 1) 0) 1 2)) '(let ((_1 (- x 1))) (let ((_2 (= _1 0))) (if _2 1 2))))
(test (to-anf '(if a (* x 1) 2)) '(if a (* x 1) 2))
(test (to-anf '(if a (* (+ x 1) 2) 2)) '(if a (let ((_1 (+ x 1))) (let ((_2 (* _1 2))) _2)) 2))
(test (to-anf '(if a b (* (+ x 1) 2))) '(if a b (let ((_1 (+ x 1))) (let ((_2 (* _1 2))) _2))))
(test (to-anf '(if (= x 0) (* (+ x 1) 2) (* (+ x 2) 3)))
      '(let ((_1 (= x 0))) (if _1
                               (let ((_2 (+ x 1))) (let ((_3 (* _2 2))) _3))
                               (let ((_4 (+ x 2))) (let ((_5 (* _4 3))) _5)))))
(test (to-anf '(begin 1)) '1)
(test (to-anf '(begin 1 2)) '(let ((_1 1)) 2))
(test (to-anf '(begin (+ x 1) (* x (+ x 2)) (* (+ x 2) 2)))
      ;; TODO: output could be improved
      '(let ((_5 (+ x 1))) (let ((_1 _5)) (let ((_6 (+ x 2))) (let ((_3 _6)) (let ((_7 (* x _3))) (let ((_4 _7)) (let ((_2 _4)) (let ((_8 (+ x 2))) (let ((_9 (* _8 2))) _9))))))))))
(test (to-anf '(cond ((= x 0) 0) ((> x 0) 1) (else -1)))
      '(let ((_1 (= x 0))) (if _1 0 (let ((_2 (> x 0))) (if _2 1 -1)))))
(test (to-anf '(cond (a (+ x (* 2 3))) (else 1)))
      '(if a (let ((_1 (* 2 3))) (let ((_2 (+ x _1))) _2)) 1))
(test (to-anf '(cond (a 1) (else (+ x (* 2 3)))))
      '(if a 1 (let ((_1 (* 2 3))) (let ((_2 (+ x _1))) _2))))
(test (to-anf ''foo) ''foo)
(test (to-anf ''(foo bar)) '(let ((_1 (cons 'bar '()))) (let ((_2 (cons 'foo _1))) _2)))
(test (eval (to-anf ''(foo (bar) (baz (qux) quux)))) '(foo (bar) (baz (qux) quux)))
(test (to-anf '(f (let ((x 1)) x))) '(let ((x 1)) (let ((_1 (f x))) _1)))
(test (to-anf '(f (lambda (x) (+ 1 (* x 2))))) '(f (lambda (x) (let ((_1 (* x 2))) (let ((_2 (+ 1 _1))) _2)))))
(test (to-anf '(f (let ((x 1)) (* x (+ x 3))) (lambda (x) (display "foo") (+ (* 3 2) x)) (equal? 'foo '(bar baz))))
      '(let ((x 1)) (let ((_9 (+ x 3))) (let ((_1 _9)) (let ((_10 (* x _1))) (let ((_2 _10)) (let ((_11 (cons 'baz '()))) (let ((_7 _11)) (let ((_12 (cons 'bar _7))) (let ((_8 _12)) (let ((_13 (equal? 'foo _8))) (let ((_14 (f _2 (lambda (x) (let ((_4 (display "foo"))) (let ((_3 _4)) (let ((_5 (* 3 2))) (let ((_6 (+ _5 x))) _6))))) _13))) _14))))))))))))

(define (convert1 exp)
  (to-anf exp))

(define (convert exps)
  (to-anf (remove-defines exps)))

(define-syntax-rule (test-eval exp)
  (check-equal? (eval exp) (eval (convert1 exp))))

(test-eval '(let ((x 42)) (set! x (+ x 1))))
(test-eval '(let ((x 42)) (+ x 1)))
(test-eval '(* (+ 4 1) 3))
(test-eval '((lambda (x) (set! x (+ x 1))) 3))
(test-eval '(let ((x 42)) (+ x (* x 2))))
(test-eval '(begin 1 2 3))
(test-eval '(letrec ((x (lambda (x) (+ (* x 2) 3)))) (x 0)))
(test-eval '((lambda (x) (+ (* x 2) 3)) 10))
(test-eval '(let ((x 1) (y 2)) y))
(test-eval '(let ((x 0)) (begin (set! x 1) (set! x 2) x)))

(require racket/cmdline)
(command-line
 #:args (filename)
 (let ((content (file->list filename)))
   (convert content)))
