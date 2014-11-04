#lang racket
;; Converts Scheme expressions to ANF by preserving most of the semantics.
;; Some cases of incompatibilities are:
;;   - let becomes equivalent to let*
;;   - no support for mutual recursion

(require racket/match)

;; '(1 2 3) -> '((1 2) . 3
(define (split-last l)
  (if (null? (cdr l))
      `(() . ,(car l))
      (match (split-last (cdr l))
        [(cons rest last) (cons (cons (car l) rest) last)])))

(define (atom? exp)
  (not (pair? exp)))

;; true for expressions that don't need to extract definitions to become atomic
(define (extract-free? exp)
  (or (atom? exp) (equal? (car exp) 'lambda)))

;; insert-in (let ((x 0)) __) x -> (let ((x 0)) x)
(define (insert-in e1 e2)
  (cond
   ((equal? e1 '__) e2)
   ((or (equal? (car e1) 'let) (equal? (car e1) 'letrec))
    `(,(car e1) ,(cadr e1) ,(insert-in (caddr e1) e2)))))

;; (insert-in '(let ((x 0)) __) 'x)
;; (insert-in '(let ((x (* 2 3))) (let ((y (+ x 1))) __)) 'y)
;; (insert-in (insert-in '(let ((x 0)) __) '(let ((y (+ x 1))) __)) 'y)

(define id 0)
(define (newid)
  (set! id (+ id 1))
  (string->symbol (string-append "_" (number->string id))))

;; (set! x (+ x 1)) -> [(let ((_x0 (+ x 1))) __) _x0]
;; ((id f) (id x)) -> [(let ((_idf (id f))) (let ((_idx (id x))) (let (_call (_idf _idx))))) _call]
(define (extract-defs exp)
  (cond
   ((extract-free? exp) (cons '__ exp))
   ((equal? (car exp) 'set!)
    (extract-defs (caddr exp)))
   ((equal? (car exp) 'if)
    (extract-defs (cadr exp)))
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

;; (extract-defs '(set! x (+ x 1)))

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
                            (insert-in acc `(let ((,(newid) ,e)) __)))
                          '__
                          exps)
                   last)])))
   ;; (let ((id e)) e) -> (let ... (let ((id ce)) e))
   ((or (equal? (car exp) 'let) (equal? (car exp) 'letrec))
    (match (extract-defs (cadr (caadr exp)))
      [(cons defs var) (insert-in defs `(,(car exp) ((,(caaadr exp) ,(to-anf var)))
                                         ,(to-anf (caddr exp))))]))
   ;; (f e...) -> (let ... (f ae...))
   (else
    (if (foldl (lambda (e acc)
                 (and acc (extract-free? e)))
               #t
               exp)
        exp
        (match (extract-defs exp)
          [(cons defs var) (insert-in defs var)])))))

;; Won't preverve semantics in the presence of mutual recursion
(define (simplify-lets exp)
  (cond
   ((atom? exp) exp)
   ((equal? (car exp) 'lambda)
    `(lambda ,(cadr exp) ,(simplify-lets (caddr exp))))
   ((equal? (car exp) 'set!)
    `(set! ,(cadr exp) ,(simplify-lets (caddr exp))))
   ((equal? (car exp) 'if)
    `(if ,(simplify-lets (cadr exp))
      ,(simplify-lets (caddr exp))
      ,(simplify-lets (cadddr exp))))
   ((or (equal? (car exp) 'let) (equal? (car exp) 'letrec))
    (let ((sym (car exp)))
      (insert-in (foldl (lambda (binding acc)
                          (insert-in acc `(,sym (,binding) __)))
                        '__
                        (cadr exp))
                 (simplify-lets (caddr exp)))))
   ((equal? (car exp) 'let*)
    (simplify-lets (cons 'let (cdr exp))))
   (else
    (map simplify-lets exp))))

;; Only works with a bunch of defines followed by one main expression
(define (remove-defines exps)
  (foldl (lambda (e acc)
           (cond
            ((equal? (car e) 'define)
             (if (pair? (cadr e))
                 (insert-in acc `(letrec ((,(caadr e) (lambda ,(cdadr e) ,(caddr e)))) __))
                 (insert-in acc `(let ((,(cadr e) ,(caddr e))) __))))
            (else (insert-in acc e))))
         '__
         exps))

;; (simplify-lets '(letrec ((x 1) (y 2)) x))

(define (convert1 exp)
  (to-anf (simplify-lets exp)))

(define (convert exps)
  (to-anf (simplify-lets (remove-defines exps))))

(define (test exp)
  (equal? (eval exp) (eval (convert1 exp))))

;; (test '(let ((x 42)) (set! x (+ x 1))))
;; (test '(let ((x 42)) (+ x 1)))
;; (test '(* (+ 4 1) 3))
;; (test '((lambda (x) (set! x (+ x 1))) 3))
;; (test '(let ((x 42)) (+ x (* x 2))))
;; (test '(begin 1 2 3))
;; (convert1 '(letrec ((x (lambda (x) (+ (* x 2) 3)))) (x 0)))
;; (convert1 '(lambda (x) (+ (* x 2) 3)))
