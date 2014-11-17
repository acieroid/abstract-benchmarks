#lang racket
;; Converts Scheme expressions to ANF by preserving most of the semantics.
;; Some cases of incompatibilities are:
;;   - let becomes equivalent to let*
;;   - no support for mutual recursion
;;   - no support (yet) for case
;;   - doesn't seem to work with internal defines
;;   - TODO: incorrectly rewrites lambdas with more than one expression inside

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
      (equal? (car exp) 'let) (equal? (car exp) 'letrec) (equal? (car exp) 'let*)))

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

;; (set! x (+ x 1)) -> [(let ((_x0 (+ x 1))) __) _x0]
;; ((id f) (id x)) -> [(let ((_idf (id f))) (let ((_idx (id x))) (let (_call (_idf _idx))))) _call]
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
   (else
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

(set! id 0)
(check-equal? (extract-defs '(set! x (+ x 1))) '((let ((_1 (+ x 1))) __) . (set! x _1)))
(check-equal? (extract-defs '((id f) (id x))) '((let ((_2 (id f))) (let ((_3 (id x))) (let ((_4 (_2 _3))) __))) . _4))
(check-equal? (extract-defs '(let ((x (set! x 1))) y)) '(__ . (let ((x (set! x 1))) y)))
(check-equal? (extract-defs '(if (> x 1) a b)) '((let ((_5 (> x 1))) __) . (if _5 a b)))
(check-equal? (extract-defs '(let ((x (set! (+ x 1)))) y)) '((let ((_6 (+ x 1))) __) . (let ((x (set! x _6))) y)))

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

(check-equal? (remove-defines '((define x 1) x)) '(let ((x 1)) x))
(check-equal? (remove-defines '((define (id x) x) (id 3))) '(letrec ((id (lambda (x) x))) (id 3)))
(check-equal? (remove-defines '((define a 1) (define b 2) (define (c x) 3) (+ a b (c 1))))
              '(let ((a 1)) (let ((b 2)) (letrec ((c (lambda (x) 3))) (+ a b (c 1))))))

;; (quote foo) -> (quote foo)
;; (quote (foo bar (baz))) -> (cons 'foo (cons 'bar (cons (cons 'baz '()) '())))
;; (quote (foo ...) -> (cons 'foo (rec ...))
;; anything else -> no modification (does not handle quotes nested inside expressions)
(define (simplify-quote exp)
  (if (and (pair? exp) (equal? (car exp) 'quote))
      (letrec ((loop (lambda (e)
                       (cond
                        ((null? e) '(quote ()))
                        ((pair? e) `(cons ,(loop (car e)) ,(loop (cdr e))))
                        (else `(quote ,e))))))
        (loop (cadr exp)))
      exp))

(check-equal? (simplify-quote ''foo) ''foo)
(check-equal? (simplify-quote ''(foo bar (baz)))
              '(cons 'foo (cons 'bar (cons (cons 'baz '()) '()))))
(check-equal? (eval (simplify-quote ''(foo bar (baz)))) '(foo bar (baz)))
(check-equal? (eval (simplify-quote ''((foo) (bar (baz (qux))) (baz (bar) (qux quux)))))
              '((foo) (bar (baz (qux))) (baz (bar) (qux quux))))
(check-equal? (simplify-quote '(if (> x 1) a b)) '(if (> x 1) a b))

(define (make-begin exps)
  (if (= (length exps) 1)
      (car exps)
      `(begin ,@exps)))

(check-equal? (make-begin '(foo)) 'foo)
(check-equal? (make-begin '(foo bar)) '(begin foo bar))

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

(check-equal? (remove-cond '(cond ((= x 1) a) ((= x 2) b) ((= x 3) c) (else d)))
              '(if (= x 1) a (if (= x 2) b (if (= x 3) c d))))
(check-equal? (remove-cond '(cond ((= x 1) a b c) (else d e f)))
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

;; (let ((x 1) (y 2)) body) -> (let ((x 1)) (let ((y 2)) (begin body)))
(check-equal? (simplify-lets '(let ((x 1) (y 2)) body)) '(let ((x 1)) (let ((y 2)) body)))
(check-equal? (simplify-lets '(let ((x 1) (y 2)) a b)) '(let ((x 1)) (let ((y 2)) (begin a b))))
(check-equal? (simplify-lets '(let* ((x 1) (y 2)) body)) '(let ((x 1)) (let ((y 2)) body))) ; get rid of let*
(check-equal? (simplify-lets '(letrec ((x 1) (y 2)) body)) '(letrec ((x 1)) (letrec ((y 2)) body)))
;; TODO:
;; (check-equal? (simplify-lets '(letrec ((x (lambda () (y))) (y (lambda () (x)))) body)
;;               '(let ((x #f)) (let ((y (lambda () (x)))) (begin (set! x (lambda () (y))) body))))

(define (to-anf exp)
  (cond
   ;; v
   ((atom? exp)
    exp)
   ;; lam
   ((equal? (car exp) 'lambda)
    `(lambda ,(cadr exp) ,(to-anf (caddr exp))))
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
   (else
    (if (foldl (lambda (e acc)
                 (and acc (extract-free? e)))
               #t
               exp)
        exp
        (match (extract-defs exp)
          [(cons defs var) (insert-in defs var)])))))


(define (convert1 exp)
  (to-anf exp))

(define (convert exps)
  (to-anf (remove-defines exps)))

(define (test exp)
  (equal? (eval exp) (eval (convert1 exp))))

;; (test '(let ((x 42)) (set! x (+ x 1))))
;; (test '(let ((x 42)) (+ x 1)))
;; (test '(* (+ 4 1) 3))
;; (test '((lambda (x) (set! x (+ x 1))) 3))
;; (test '(let ((x 42)) (+ x (* x 2))))
;; (test '(begin 1 2 3))
;; (test '(letrec ((x (lambda (x) (+ (* x 2) 3)))) (x 0)))
;; (test '((lambda (x) (+ (* x 2) 3)) 10))
;; (test '(let ((x 1) (y 2)) y))
;; (test '(let ((x 0)) (begin (set! x 1) (set! x 2) x)))

(require racket/cmdline)
(command-line
 #:args (filename)
 (let ((content (file->list filename)))
   (convert content)))
