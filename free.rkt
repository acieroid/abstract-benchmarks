#lang racket
;; Returns the free variables of an expression. Useful to know what are the
;; minimal set of functions needed to run an example.
;; This is meant to run on ANF code.

(require racket/match)
(require racket/set)

(define (free exp)
  (match exp
    [`(lambda ,args ,body)
     (set-subtract (free body) args)]
    [`(if ,condition ,cons ,alt)
     (set-union (free condition) (free cons) (free alt))]
    [`(set! ,v ,ae)
     (set-add (free ae) v)]
    [`(let ((,id ,val)) ,body)
     (set-union (set-remove (free body) id) (free val))]
    [`(letrec ((,id ,val)) ,body)
     (set-remove (set-union (free body) (free val)) id)]
    [`(,fun . ,args)
     (foldl set-union
            (list->set '())
            (map free (cons fun args)))]
    [atom
     (list->set (if (or (number? atom) (string? atom))
                    (list)
                    (list atom)))]))
