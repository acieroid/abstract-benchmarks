#lang racket
;; Returns the free variables of an expression. Useful to know what are the
;; minimal set of functions needed to run an example.
;; This is meant to run on ANF code.
;; TODO: does not mix well with code that uses symbols

(require racket/match)
(require racket/set)

(define (free exp)
  (match exp
    [`(lambda ,args ,body)
     (set-subtract (free body) (list->set args))]
    [`(if ,condition ,cons ,alt)
     (set-union (free condition) (free cons) (free alt))]
    [`(set! ,v ,ae)
     (set-add (free ae) v)]
    [`(let ((,id ,val)) ,body)
     (set-union (set-remove (free body) id) (free val))]
    [`(letrec ((,id ,val)) ,body)
     (set-remove (set-union (free body) (free val)) id)]
    [`(quote ,_)
     (set)]
    [`(,fun . ,args)
     (foldl set-union
            (set)
            (map free (cons fun args)))]
    [atom
     (if (or (number? atom) (string? atom))
         (set)
         (set atom))]))

(require racket/cmdline)
(command-line
 #:args (filename)
 (let ((content (file->list filename)))
   (display (free content))))
