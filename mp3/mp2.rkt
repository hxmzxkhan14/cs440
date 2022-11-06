#lang racket
(require racket/trace)

(provide parse
         desugar
         eval
         load-defs
         repl)


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; relational operators
(struct rel-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; booleans
(struct bool-exp (bool) #:transparent)

;; strings
(struct str-exp (str) #:transparent)

;; if expression
(struct if-exp (bexp texp fexp) #:transparent)

;; and expression
(struct and-exp (args) #:transparent)

;; or expression
(struct or-exp (args) #:transparent)

;; cond expression
(struct cond-exp (args vals) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

;; function value + closure
(struct fun-val (id body env) #:prefab)

;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; identifier (variable)
    [(? symbol?)
     (var-exp sexp)]

    ;; booleans
    [(? boolean?)
     (bool-exp sexp)]

    ;; strings
    [(? string?)
     (str-exp sexp)]

    ;; arithmetic expression
    [(list (and op (or '+ '* '-)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; relational expressions
    [(list (and op (or '= '< '> '>= '<=)) lhs rhs)
     (rel-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; if expressions
    [(list 'if bexp texp fexp)
     (if-exp (parse bexp) (parse texp) (parse fexp))]

    ;; and expressions
    [(list 'and args ...)
     (and-exp (map parse args))]

    ;; or expressions
    [(list 'or args ...)
     (or-exp (map parse args))]

    ;; cond expressions
    [(list 'cond (list arg val) ...)
     (cond-exp (map parse arg) (map parse val))]
   
    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; lambda expression -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))

;(trace parse)


;; Desugar-er -- i.e., syntax transformer
(define (desugar exp)
  (match exp

    ((bool-exp exp)
     (bool-exp exp))
    
    ((arith-exp op lhs rhs)
     (cond
       ;; subtraction
       [(equal? "-" op)
        (arith-exp "+" (desugar lhs) (arith-exp "*" (parse -1) (desugar rhs)))]
       [else
        (arith-exp op (desugar lhs) (desugar rhs))]))

    ; if desugaring for desugaring function bodies in test2.defs
    ((if-exp bool texp fexp)
     (if-exp (desugar bool) (desugar texp) (desugar fexp)))

    ;; and desugaring
    ((and-exp args)
     (cond
       [(empty? args) (parse #t)]
       [else
        (if-exp (first args) (desugar (and-exp (rest args))) (parse #f))]))
    
    ;; or desugaring
    ((or-exp args)
     (cond
       [(empty? args) (parse #f)]
       [else
        (if-exp (first args) (parse #t) (desugar (or-exp (rest args))))]))
    
    ;; cond desugaring
    ((cond-exp args vals)
     (cond
       [(var-exp? (first args)) (first vals)]
       [else
        (if-exp (first args) (first vals) (desugar (cond-exp (rest args) (rest vals))))]))

    ;; relational sugar
    ((rel-exp op lhs rhs)
     (cond
       [(equal? "=" op) (rel-exp op lhs rhs)]
       [(equal? "<" op) (rel-exp op lhs rhs)]
       [(equal? ">=" op) (desugar (or-exp (list (rel-exp "<" (desugar rhs) (desugar lhs)) (rel-exp "=" (desugar lhs) (desugar rhs)))))]
       [(equal? "<=" op) (desugar (or-exp (list (rel-exp "<" (desugar lhs) (desugar rhs)) (rel-exp "=" (desugar lhs) (desugar rhs)))))]
       [(equal? ">" op) (if-exp (rel-exp "<" (desugar rhs) (desugar lhs)) (parse #t) (parse #f))]))

    ((let-exp ids vals body)
     (let-exp ids (map desugar vals) (desugar body)))
    
    ((lambda-exp ids body)
     (foldr (lambda (id lexp) (lambda-exp id lexp))
            (desugar body)
            ids))
    
    ((app-exp f args)
     (foldl (lambda (id fexp) (app-exp fexp id))
            (desugar f)
            (map desugar args)))
    
    ((list args)
     (desugar args))

    (_ exp)))

;(trace desugar)


;; Interpreter
(define (eval expr [env '()])
  (match expr
    
    ;; int literal
    [(int-exp val) val]

    ;; booleans
    [(bool-exp bool) bool]

    ;; strings
    [(str-exp str) str] 

    ;; arithmetic expression
    [(arith-exp "+" lhs rhs)
     (+ (eval lhs env) (eval rhs env))]
    [(arith-exp "*" lhs rhs)
     (* (eval lhs env) (eval rhs env))]

    ;; relational operators
    [(rel-exp "=" lhs rhs)
     (cond
       [(equal? (eval lhs env) (eval rhs env)) #t]
       [else #f])]
    [(rel-exp "<" lhs rhs)
     (cond
       [(< (eval lhs env) (eval rhs env)) #t]
       [else #f])]
          
    ;; variable binding
    [(var-exp id)
     (let ([pair (assoc id env)])
       (if pair (cdr pair) (error (format "~a not bound!" id))))]

    ;; if expression
    [(if-exp bool-exp true-exp false-exp)
     (if (eval bool-exp env)
         (eval true-exp env)
         (eval false-exp env))]

    ;; let expression
    [(let-exp (list (var-exp id) ...) (list val ...) body)
     (let ([vars (map cons id
                      (map (lambda (v) (eval v env)) val))])
       (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)
     (fun-val id body env)]

    ;; function application
    [(app-exp f arg)
     (match-let ([(fun-val id body clenv) (eval f env)]
                  [arg-val (eval arg env)])
       (eval body (cons (cons id arg-val) clenv)))]

       
    [(list args)
     (eval args env)]
    
    ;; basic error handling
    [_ (error (format "Can't evaluate: ~a" expr))]))

;(trace eval)

(define (load-defs filename)
      (let ([sexps (file->list filename)])
        (if (equal? (length sexps) 3)
            (let* ([fns (map (compose first second) sexps)]
                   [ids (map (compose second second) sexps)]
                   [bodies (map parse (map third sexps))]
                   [ph (make-placeholder '())]
                   [env (list (cons (first fns) (fun-val (first ids) (first bodies) ph))
                              (cons (second fns) (fun-val (second ids) (second bodies) ph))
                              (cons (third fns) (fun-val (third ids) (third bodies) ph)))])
              (placeholder-set! ph env)
              (make-reader-graph env))
            (let* ([fns (map (compose first second) sexps)]
                   [ids (map (compose second second) sexps)]
                   [bodies (map parse (map third sexps))]
                   [ph (make-placeholder '())]
                   [env (list (cons (first fns) (fun-val (first ids) (desugar (first bodies)) ph))
                              (cons (second fns) (fun-val (second ids) (desugar (second bodies)) ph))
                              (cons (third fns) (fun-val (third ids) (desugar (third bodies)) ph))
                              (cons (fourth fns) (fun-val (fourth ids) (desugar (fourth bodies)) ph))
                              (cons (fifth fns) (fun-val (fifth ids) (desugar (fifth bodies)) ph)))])
              (placeholder-set! ph env)
              (make-reader-graph env)))))

;(trace load-defs)
      
;; REPL
(define (repl [filename #f])
  (let loop ([env (if filename (load-defs filename) '())])
    (let ([stx (desugar (parse (read)))])
      (when stx
        (println (eval stx env))
        (loop env)))))