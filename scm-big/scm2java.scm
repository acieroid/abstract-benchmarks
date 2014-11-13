;; Expected result: "public class BOut extends RuntimeEnvironment {\n public static void main (String[] args) {\nnew IntValue(3) ;\n }\n}\n"
;; A Scheme-to-Java compiler.

;; The compiler is designed to show how close
;; the mapping between Scheme and Java can be.

;; Author: Matthew Might
;; Site:   http://matt.might.net/
;;         http://www.ucombinator.org/

;; The input language contains integers, variables,
;; a few primitives, lambda terms, let terms, explicit
;; recursion (letrec), conditionals and function
;; applications, sequencing and mutable variables.

;; <exp> ::= <const>
;;        |  <prim>
;;        |  <var>
;;        |  (lambda (<var> ...) <exp>)
;;        |  (if <exp> <exp> <exp>)
;;        |  (set! <var> <exp>)
;;        |  (let ((<var> <exp>) ...) <exp>)
;;        |  (letrec ((<var> (lambda (<var>...) <exp>))) <exp>)
;;        |  (begin <exp> ...)
;;        |  (<exp> <exp> ...)

;; <const> ::= <int>

;; To run this compiler, run an R5RS-compatible interpreter
;; on this file and pipe a Scheme expression into stdin:

;;  $ interpret thisfile.scm < input.scm > BOut.java
;;  $ javac Value.java BOut.java 
;;  $ java BOut

;; The file Value.java is required to compile the output.
;; Value.java defines internal data types as well as the
;; runtime environment.

;; To handle closures, the compiler uses Java's
;; anonymous class mechanism.

;; To handle recursion, the compiler creates a Y
;; combinator with the appropriate arity.

;; To handle mutable variables, the compiler first
;; analyzes programs to find the set!'d names.
;; Mutable names are then wrapped in ValueCell objects.

;; This compiler is reasonably close to meta-circular.
;; With a few modifications and an s-expression parser
;; in Java, it would be.



;; Utilities.

(define (cadr p) (car (cdr p)))
(define (caadr p) (car (car (cdr p))))
(define (caddr p) (car (cdr (cdr p))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))

(define (map f lst) 
  (if (pair? lst)
      (cons (f (car lst))
            (map f (cdr lst)))
      '()))

(define (append lst1 lst2)
  (if (not (pair? lst1))
      lst2
      (cons (car lst1) 
            (append (cdr lst1) lst2))))

(define (string->list s)
  (letrec ((f (lambda (i)
                (if (< i (string-length s))
                    (cons (string-ref s i)
                          (f (+ i 1)))
                    '()))))
    (f 0)))

; void : -> void
(define (void) #f)

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))



;; Data type predicates and accessors.

; const? : exp -> boolean
(define (const? exp)
  (integer? exp))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (caddr exp))

; letrec1? : exp -> boolean
(define (letrec1? exp)
  (and (tagged-list? 'letrec exp)
       (= (length (cadr exp)) 1)))

; letrec1->binding : letrec1-exp -> (symbol exp)
(define (letrec1->binding exp)
  (caadr exp))

; letrec1->exp : letrec1-exp -> exp
(define (letrec1->exp exp)
  (caddr exp))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (caddr exp))

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
; prim? : exp -> boolean
(define (prim? exp)
  (or (eq? exp '+)
      (eq? exp '-)
      (eq? exp '*)
      (eq? exp '=)
      (eq? exp 'display)))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; begin-i>exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!-var : set!-exp -> var
(define (set!-var exp)
  (cadr exp))

; set!-exp : set!-exp -> exp
(define (set!-exp exp)
  (caddr exp))



;; Desugarings.

; let=>lambda : let-exp -> app-exp
(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        (cons (cons 'lambda (cons vars (cons (let->exp exp) '()))) args))
      exp))


; arity : lambda-exp -> nat
(define (arity lam)
  (length (lambda->formals lam)))

; xargs : nat -> list[symbol]
(define (xargs n)
  (if (<= n 0)
      '()
      (cons (string->symbol (string-append "x" (number->string n)))
            (xargs (- n 1)))))
       
; Yn generates the Y combinator for n-arity procedures.
(define (Yn n)
  ;; Was:
  ;; `((lambda (h) (lambda (F) (F (lambda (,@(xargs n)) (((h h) F) ,@(xargs n))))))
  ;;   (lambda (h) (lambda (F) (F (lambda (,@(xargs n)) (((h h) F) ,@(xargs n)))))))
  (cons
   (cons 'lambda
         (cons (cons 'h '())
               (cons
                (cons 'lambda
                      (cons (cons 'F '())
                            (cons (cons 'F
                                        (cons (cons 'lambda
                                                    (cons (xargs n)
                                                          (cons
                                                           (cons
                                                            (cons (cons 'h (cons 'h '())) (cons 'F '()))
                                                            (xargs n))
                                                           '())))
                                              '()))
                                  '())))
                '())))
   (cons (cons 'lambda
               (cons (cons 'h '())
                     (cons
                      (cons 'lambda
                            (cons (cons 'F '())
                                  (cons (cons 'F
                                              (cons (cons 'lambda
                                                          (cons (xargs n)
                                                                (cons
                                                                 (cons
                                                                  (cons (cons 'h (cons 'h '())) (cons 'F '()))
                                                                  (xargs n))
                                                                 '())))
                                                    '()))
                                        '())))
                      '())))
         '())))

; letrec1=>Y : letrec1-exp -> let-exp
(define (letrec1=>Y exp)
  (if (letrec1? exp)
      (let* ((binding  (letrec1->binding exp))
             (name     (car binding))
             (arg      (cadr binding))
             (num-args (arity arg)))
        ;; Was:
        ;; `(let ((,name (,(Yn num-args) (lambda (,name) ,arg))))
        ;;    ,(letrec1->exp exp))
        (cons 'let
              (cons
               (cons (cons name
                            (cons (cons (Yn num-args)
                                        (cons (cons 'lambda (cons (cons name '()) (cons arg '())))
                                              '()))
                                  '()))
                     '())
               (cons (letrec1-exp exp) '()))))
      exp))
        
; begin=>let : begin-exp -> let-exp
(define (singlet? l)
  (and (list? l)
       (= (length l) 1)))

(define (dummy-bind exps)
  (if (singlet? exps)
      (car exps)
      (if (pair? exps)
          ;; Was:
          ;; `(let (($_ ,(car exps)))
          ;;    ,(dummy-bind (cdr exps)))
          (cons 'let
                (cons
                 (cons (cons '$_
                             (cons (car exps) '()))
                       '())
                 (cons (dummy-bind (cdr exps)) '())))
          (error "no match"))))

(define (begin=>let exp)
  (dummy-bind (begin->exps exp)))



;; Mutable variable analysis.

;; Variables which are mutable are 
;; wrapped in ValueCell objects.

; mutable-variables : list[symbol]
(define mutable-variables '())

; mark-mutable : symbol -> void
(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

; is-mutable? : symbol -> boolean
(define (is-in? S symbol)
  (if (not (pair? S))
      #f
      (if (eq? (car S) symbol)
          #t
          (is-in? (cdr S) symbol))))

(define (is-mutable? symbol)
  (is-in? mutable-variables symbol))

; analyze-mutable-variables : exp -> void
(define (analyze-mutable-variables exp)
  (if (const? exp) (void)
      (if (ref? exp) (void)
          (if (prim? exp) (void)
              (if (lambda? exp) (analyze-mutable-variables (lambda->exp exp))
                  (if (let? exp) (begin
                                   (map analyze-mutable-variables (map cadr (let->bindings exp)))
                                   (analyze-mutable-variables (let->exp exp)))
                      (if (letrec1? exp) (begin (analyze-mutable-variables (cadr (letrec1->binding exp)))
                                                 (analyze-mutable-variables (letrec1->exp exp)))
                          (if (set!? exp) (mark-mutable (set!-var exp))
                              (if (if? exp) (begin (analyze-mutable-variables (if->condition exp))
                                                   (analyze-mutable-variables (if->then exp))
                                                   (analyze-mutable-variables (if->else exp)))
                                  (if (begin? exp) (begin (map analyze-mutable-variables (begin->exps exp))
                                                          (void))
                                      (if (app? exp) (begin (map analyze-mutable-variables exp)
                                                            (void))
                                          (error "unknown expression type: " exp))))))))))))



;; Name-mangling.

;; We have to "mangle" Scheme identifiers into
;; Java-compatible identifiers, because names like
;; foo-bar/baz are not identifiers in Java.

; mangle : symbol -> string
(define (m chars)
  (if (null? chars)
      '()
      (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
              (char-numeric? (car chars)))
          (cons (car chars) (m (cdr chars)))
          (cons #\_ (append (integer->char-list (char->natural (car chars)))
                            (m (cdr chars)))))))

(define (mangle symbol)
  (list->string (m (string->list (symbol->string symbol)))))



;; Compilation routines.

; java-compile-const : const-exp -> string
(define (java-compile-const exp)
  (if (integer? exp)
      (string-append "new IntValue(" (number->string exp) ")")
      (error "unknown constant: " exp)))

; java-compile-prim : prim-exp -> string
(define (java-compile-prim p)
  (if (eq? '+ p) "sum"
      (if (eq? '- p) "difference"
          (if (eq? '* p) "product"
              (if (eq? '= p) "numEqual"
                  (if (eq? 'display p) "display"
                      (error "unhandled primitive " p)))))))

; java-compile-ref : ref-exp -> string
(define (java-compile-ref exp)
  (if (is-mutable? exp)
      (string-append "m_" (mangle exp) ".value")
      (mangle exp)))

; java-compile-formals : list[symbol] -> string
(define (java-compile-formals formals)
  (if (not (pair? formals))
      ""
      (string-append
       "final Value "
       (mangle (car formals))
       (if (pair? (cdr formals))
           (string-append ", " (java-compile-formals (cdr formals)))
           ""))))
 
; java-compile-lambda : lambda-exp -> string
(define (java-wrap-mutables vars)
  (if (not (pair? vars))
      ""
      (string-append
       (if (is-mutable? (car vars))
           (string-append 
            " final ValueCell m_" (mangle (car vars)) 
            " = new ValueCell(" (mangle (car vars)) ");\n")
           "")
       (java-wrap-mutables (cdr vars)))))

(define (java-compile-lambda exp)
  (let* ((formals (lambda->formals exp))
         (num-args (length formals)))
    (string-append
     "new NullProcValue" (number->string num-args) " () {\n"
     " public Value apply(" (java-compile-formals formals) ") {\n"
                                        ; wrap mutables in ValueCell objects:
     (java-wrap-mutables formals)
     "\n"
     "  return " (java-compile-exp (lambda->exp exp)) " ;\n"
     "}}\n")))

; java-compile-args : list[exp] -> string
(define (java-compile-args args)
  (if (not (pair? args))
      ""
      (string-append
       (java-compile-exp (car args))
       (if (pair? (cdr args))
           (string-append ", " (java-compile-args (cdr args)))
           ""))))

; java-compile-set! : set!-exp -> string
(define (java-compile-set! exp)
  (string-append "VoidValue.Void(m_"
                 (mangle (set!-var exp))
                 ".value = "
                 (java-compile-exp (set!-exp exp))
                 ")"))

; java-compile-app : app-exp -> string
(define (java-compile-app exp)
  (let* ((args     (app->args exp))
         (fun      (app->fun exp))
         (num-args (length args)))
    (string-append
     "((ProcValue" (number->string num-args) ")(" 
     (java-compile-exp fun) ")).apply(" 
     (java-compile-args args) ")\n")))

; java-compile-if : if-exp -> string
(define (java-compile-if exp)
  (string-append
   "(" (java-compile-exp (if->condition exp)) ").toBoolean() ? (" 
       (java-compile-exp (if->then exp)) ") : ("
       (java-compile-exp (if->else exp)) ")"))


; java-compile-exp : exp -> string
(define (java-compile-exp exp)
  ; core forms:
  (if (const? exp) (java-compile-const exp)
      (if (prim?  exp) (java-compile-prim exp)
          (if (ref?   exp) (java-compile-ref exp)
              (if (lambda? exp) (java-compile-lambda exp)
                  (if (if? exp) (java-compile-if exp)
                      (if (set!? exp) (java-compile-set! exp)
                          ; syntactic sugar:
                          (if (let? exp) (java-compile-exp (let=>lambda exp))
                              (if (letrec1? exp) (java-compile-exp (letrec1=>Y exp))
                                  (if (begin? exp) (java-compile-exp (begin=>let exp))
                                      ; applications:
                                      (if (app? exp) (java-compile-app exp)
                                          (error "no match"))))))))))))



; java-compile-program : exp -> string
(define (java-compile-program exp)
  (string-append 
   "public class BOut extends RuntimeEnvironment {\n"
   " public static void main (String[] args) {\n"
   (java-compile-exp exp) 
   " ;\n"
   " }\n"
   "}\n"))

;; Read in an expression, compile it, and print it out:

;; Hard-coded program for static analysis benchmarking.
(define input-program 3)

(begin (analyze-mutable-variables input-program)
       (java-compile-program input-program))

;; The resulting program requires Value.java to compile.
