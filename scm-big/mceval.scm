;; Expected result: 40320
;; Meta-circular evaluator from SICP

(let ((self-evaluating? (lambda (exp)
                          (cond ((number? exp) #t)
                                ((string? exp) #t)
                                (else #f)))))
  (let ((variable? (lambda (exp) (symbol? exp))))
    (let ((tagged-list? (lambda (exp tag)
                          (if (pair? exp)
                              (eq? (car exp) tag)
                              #f))))
      (let ((quoted? (lambda (exp)
                       (tagged-list? exp 'quote))))
        (let ((text-of-quotation (lambda (exp) (cadr exp))))
          (let ((assignment? (lambda (exp) (tagged-list? exp 'set!))))
            (let ((assignment-variable (lambda (exp) (cadr exp))))
              (let ((assignment-value (lambda (exp) (caddr exp))))

                (let ((definition? (lambda (exp) (tagged-list? exp 'define))))
                  (let ((definition-variable (lambda (exp) (if (symbol? (cadr exp))
                                                               (cadr exp)
                                                               (caadr exp)))))
                    (let ((make-lambda (lambda (parameters body)
                                         (cons 'lambda (cons parameters body)))))
                      (let ((definition-value (lambda (exp)
                                                (if (symbol? (cadr exp))
                                                    (caddr exp)
                                                    (make-lambda (cdadr exp) ; formal parameters
                                                                 (cddr exp))))) ; body
                            )
                        (let ((lambda? (lambda (exp) (tagged-list? exp 'lambda))))
                          (let ((lambda-parameters (lambda (exp) (cadr exp))))
                            (let ((lambda-body (lambda (exp) (cddr exp))))

                              (let ((if? (lambda (exp) (tagged-list? exp 'if))))
                                (let ((if-predicate (lambda (exp) (cadr exp))))
                                  (let ((if-consequent (lambda (exp) (caddr exp))))
                                    (let ((if-alternative (lambda (exp)
                                                            (if (not (null? (cdddr exp)))
                                                                (cadddr exp)
                                                                'false))))
                                      (let ((make-if (lambda (predicate consequent alternative)
                                                       (cons 'if (cons predicate (cons consequent (cons alternative '())))))))
                                        (let ((begin? (lambda (exp) (tagged-list? exp 'begin))))
                                          (let ((begin-actions (lambda (exp) (cdr exp))))
                                            (let ((last-exp? (lambda (seq) (null? (cdr seq)))))
                                              (let ((first-exp (lambda (seq) (car seq))))
                                                (let ((rest-exps (lambda (seq) (cdr seq))))

                                                  (let ((mk-begin (lambda (seq) (cons 'begin seq))))
                                                    (let ((sequence->exp (lambda (seq)
                                                                           (cond ((null? seq) seq)
                                                                                 ((last-exp? seq) (first-exp seq))
                                                                                 (else (mk-begin seq))))))

                                                      (let ((application? (lambda (exp) (pair? exp))))
                                                        (let ((operator (lambda (exp) (car exp))))
                                                          (let ((operands (lambda (exp) (cdr exp))))
                                                            (let ((no-operands? (lambda (ops) (null? ops))))
                                                              (let ((first-operand (lambda (ops) (car ops))))
                                                                (let ((rest-operands (lambda (ops) (cdr ops))))
                                                                  (let ((cond? (lambda (exp) (tagged-list? exp 'cond))))
                                                                    (let ((cond-clauses (lambda (exp) (cdr exp))))
                                                                      (let ((cond-predicate (lambda (clause) (car clause))))
                                                                        (let ((cond-else-clause? (lambda (clause)
                                                                                                   (eq? (cond-predicate clause) 'else))))
                                                                          (let ((cond-actions (lambda (clause) (cdr clause))))
                                                                            (letrec ((expand-clauses (lambda (clauses)
                                                                                                       (if (null? clauses)
                                                                                                           'false ; no else clause
                                                                                                           (let ((first (car clauses))
                                                                                                                 (rest (cdr clauses)))
                                                                                                             (if (cond-else-clause? first)
                                                                                                                 (if (null? rest)
                                                                                                                     (sequence->exp (cond-actions first))
                                                                                                                     (error "ELSE clause isn't last -- COND->IF"
                                                                                                                            clauses))
                                                                                                                 (make-if (cond-predicate first)
                                                                                                                          (sequence->exp (cond-actions first))
                                                                                                                          (expand-clauses rest))))))))
                                                                              (let ((cond->if (lambda (exp) (expand-clauses (cond-clauses exp)))))

                                                                                (let ((true? (lambda (x) (not (eq? x #f)))))
                                                                                  (let ((false? (lambda (x) (eq? x #f))))

                                                                                    (let ((make-procedure (lambda (parameters body env) (cons 'procedure (cons parameters (cons body (cons env '())))))))
                                                                                      (let ((compound-procedure? (lambda (p) (tagged-list? p 'procedure))))
                                                                                        (let ((procedure-parameters (lambda (p) (cadr p))))
                                                                                          (let ((procedure-body (lambda (p) (caddr p))))
                                                                                            (let ((procedure-environment (lambda (p) (cadddr p))))
                                                                                              (let ((enclosing-environment (lambda (env) (cdr env))))
                                                                                                (let ((first-frame (lambda (env) (car env))))
                                                                                                  (let ((the-empty-environment '()))

                                                                                                    (let ((make-frame (lambda (variables values) (cons variables values))))
                                                                                                      (let ((frame-variables (lambda (frame) (car frame))))
                                                                                                        (let ((frame-values (lambda (frame) (cdr frame))))
                                                                                                          (let ((add-binding-to-frame! (lambda (var val frame)
                                                                                                                                         (set-car! frame (cons var (car frame)))
                                                                                                                                         (set-cdr! frame (cons val (cdr frame)))
                                                                                                                                         )))
                                                                                                            (let ((extend-environment (lambda (vars vals base-env)
                                                                                                                                        (if (= (length vars) (length vals))
                                                                                                                                            (cons (make-frame vars vals) base-env)
                                                                                                                                            (if (< (length vars) (length vals))
                                                                                                                                                (error "Too many arguments supplied" vars vals)
                                                                                                                                                (error "Too few arguments supplied" vars vals))))))

                                                                                                              (let ((lookup-variable-value (lambda (var env)
                                                                                                                                             (letrec ((env-loop (lambda (env)
                                                                                                                                                                  (letrec ((scan (lambda (vars vals)
                                                                                                                                                                                   (cond ((null? vars)
                                                                                                                                                                                          (env-loop (enclosing-environment env)))
                                                                                                                                                                                         ((eq? var (car vars))
                                                                                                                                                                                          (car vals))
                                                                                                                                                                                         (else (scan (cdr vars) (cdr vals)))))))
                                                                                                                                                                    (if (eq? env the-empty-environment)
                                                                                                                                                                        (error "Unbound variable" var)
                                                                                                                                                                        (let ((frame (first-frame env)))
                                                                                                                                                                          (scan (frame-variables frame)
                                                                                                                                                                                (frame-values frame))))))))
                                                                                                                                               (env-loop env)))))
                                                                                                                (let ((set-variable-value! (lambda (var val env)
                                                                                                                                             (letrec ((env-loop (lambda (env)
                                                                                                                                                                  (letrec ((scan (lambda (vars vals)
                                                                                                                                                                                   (cond ((null? vars)
                                                                                                                                                                                          (env-loop (enclosing-environment env)))
                                                                                                                                                                                         ((eq? var (car vars))
                                                                                                                                                                                          (set-car! vals val))
                                                                                                                                                                                         (else (scan (cdr vars) (cdr vals)))))))
                                                                                                                                                                    (if (eq? env the-empty-environment)
                                                                                                                                                                        (error "Unbound variable -- SET!" var)
                                                                                                                                                                        (let ((frame (first-frame env)))
                                                                                                                                                                          (scan (frame-variables frame)
                                                                                                                                                                                (frame-values frame))))))))
                                                                                                                                               (env-loop env)))))

                                                                                                                  (let ((define-variable! (lambda (var val env)
                                                                                                                                            (let ((frame (first-frame env)))
                                                                                                                                              (letrec ((scan (lambda (vars vals)
                                                                                                                                                               (cond ((null? vars)
                                                                                                                                                                      (add-binding-to-frame! var val frame))
                                                                                                                                                                     ((eq? var (car vars))
                                                                                                                                                                      (set-car! vals val))
                                                                                                                                                                     (else (scan (cdr vars) (cdr vals)))))))
                                                                                                                                                (scan (frame-variables frame)
                                                                                                                                                      (frame-values frame)))))))

                                                                                                                    (let ((primitive-procedure? (lambda (proc) (tagged-list? proc 'primitive))))

                                                                                                                      (let ((primitive-implementation (lambda (proc) (cadr proc))))

                                                                                                                        (let ((primitive-procedures
                                                                                                                               (cons (cons '= (cons = '()))
                                                                                                                                     (cons (cons '* (cons * '()))
                                                                                                                                           (cons (cons '- (cons - '())) '())))))
                                                                                                                          (let ((primitive-procedure-names (lambda ()
                                                                                                                                                             (map car
                                                                                                                                                                  primitive-procedures))))

                                                                                                                            (let ((primitive-procedure-objects (lambda ()
                                                                                                                                                                 (map (lambda (proc) (cons 'primitive (cons (cadr proc) '())))
                                                                                                                                                                      primitive-procedures))))
                                                                                                                              (let ((setup-environment (lambda ()
                                                                                                                                                         (let ((initial-env
                                                                                                                                                                (extend-environment (primitive-procedure-names)
                                                                                                                                                                                    (primitive-procedure-objects)
                                                                                                                                                                                    the-empty-environment)))
                                                                                                                                                           (define-variable! 'true #t initial-env)
                                                                                                                                                           (define-variable! 'false #f initial-env)
                                                                                                                                                           initial-env))))
                                                                                                                                (let ((the-global-environment (setup-environment)))

                                                                                                                                  (let ((apply-primitive-procedure (lambda (proc args)
                                                                                                                                                                     (let ((f (primitive-implementation proc))
                                                                                                                                                                           (n (length args)))
                                                                                                                                                                       (cond ((= n 0) (f))
                                                                                                                                                                             ((= n 1) (f (car args)))
                                                                                                                                                                             ((= n 2) (f (car args) (cadr args)))
                                                                                                                                                                             (else (error "ERROR -- can't handle more than two arguments")))))))

                                                                                                                                    (letrec ((mceval (lambda (exp env)
                                                                                                                                                       (letrec ((eval-sequence (lambda (exps env)
                                                                                                                                                                                 (cond ((last-exp? exps) (mceval (first-exp exps) env))
                                                                                                                                                                                       (else (mceval (first-exp exps) env)
                                                                                                                                                                                             (eval-sequence (rest-exps exps) env))))))
                                                                                                                                                         (let ((mcapply (lambda (procedure arguments)
                                                                                                                                                                          (cond ((primitive-procedure? procedure)
                                                                                                                                                                                 (apply-primitive-procedure procedure arguments))
                                                                                                                                                                                ((compound-procedure? procedure)
                                                                                                                                                                                 (eval-sequence
                                                                                                                                                                                  (procedure-body procedure)
                                                                                                                                                                                  (extend-environment
                                                                                                                                                                                   (procedure-parameters procedure)
                                                                                                                                                                                   arguments
                                                                                                                                                                                   (procedure-environment procedure))))
                                                                                                                                                                                (else
                                                                                                                                                                                 (error
                                                                                                                                                                                  "Unknown procedure type -- APPLY" procedure))))))
                                                                                                                                                           (let ((eval-if (lambda (exp env)
                                                                                                                                                                            (if (true? (mceval (if-predicate exp) env))
                                                                                                                                                                                (mceval (if-consequent exp) env)
                                                                                                                                                                                (mceval (if-alternative exp) env)))))
                                                                                                                                                             (let ((eval-assignment (lambda (exp env)
                                                                                                                                                                                      (set-variable-value! (assignment-variable exp)
                                                                                                                                                                                                           (mceval (assignment-value exp) env)
                                                                                                                                                                                                           env)
                                                                                                                                                                                      'ok)))
                                                                                                                                                               (let ((eval-definition (lambda (exp env)
                                                                                                                                                                                        (define-variable! (definition-variable exp)
                                                                                                                                                                                          (mceval (definition-value exp) env)
                                                                                                                                                                                          env)
                                                                                                                                                                                        'ok)))
                                                                                                                                                                 (letrec ((list-of-values (lambda (exps env)
                                                                                                                                                                                            (if (no-operands? exps)
                                                                                                                                                                                                '()
                                                                                                                                                                                                (cons (mceval (first-operand exps) env)
                                                                                                                                                                                                      (list-of-values (rest-operands exps) env))))))
                                                                                                                                                                 (cond ((self-evaluating? exp) exp)
                                                                                                                                                                       ((variable? exp) (lookup-variable-value exp env))
                                                                                                                                                                       ((quoted? exp) (text-of-quotation exp))
                                                                                                                                                                       ((assignment? exp) (eval-assignment exp env))
                                                                                                                                                                       ((definition? exp) (eval-definition exp env))
                                                                                                                                                                       ((if? exp) (eval-if exp env))
                                                                                                                                                                       ((lambda? exp)
                                                                                                                                                                        (make-procedure (lambda-parameters exp)
                                                                                                                                                                                        (lambda-body exp)
                                                                                                                                                                                        env))
                                                                                                                                                                       ((begin? exp)
                                                                                                                                                                        (eval-sequence (begin-actions exp) env))
                                                                                                                                                                       ((cond? exp) (mceval (cond->if exp) env))
                                                                                                                                                                       ((application? exp)
                                                                                                                                                                        (mcapply (mceval (operator exp) env)
                                                                                                                                                                                 (list-of-values (operands exp) env)))
                                                                                                                                                                       (else
                                                                                                                                                                        (error "Unknown expression type -- EVAL" exp))))))))))))

                                                                                                                                      (mceval '(define (fac n)
                                                                                                                                                 (if (= n 0)
                                                                                                                                                     1
                                                                                                                                                     (* n (fac (- n 1)))))
                                                                                                                                              the-global-environment)

                                                                                                                                      (mceval '(fac 8)
                                                                                                                                              the-global-environment))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
