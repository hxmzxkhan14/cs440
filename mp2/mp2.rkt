#lang racket

(provide (all-defined-out)) ; export all top-level definitions
(require racket/trace)

;;;;; Part 1: HOFs (and some utility functions)

(define (deep-map fn lst)
  (cond
    [(list? lst)
     (if (empty? lst)
         '() 
         (cons (deep-map fn (first lst)) (deep-map fn (rest lst))))]
    [(pair? lst) (cons (deep-map fn (car lst)) (deep-map fn (cdr lst)))]
    [else (fn lst)]))

;(trace deep-map)

(define (my-curry fn . rest)
  (cond
    [(equal? (procedure-arity fn) 0) (lambda remain (apply fn rest))]
    [(equal? (procedure-arity fn) (length rest)) (apply fn rest)]
    [else (lambda remain (apply my-curry fn (append rest remain)))]))

(define (lookup key lst)
  (cond 
     [(= (length lst) 0) #f]
     [(eq? key (car (car lst))) (cdr (car lst))]
     [else (lookup key (rest lst))]))


(define (update key value lst)
  (cond
    [(= (length lst) 0) (append (list (cons key value)) lst)]
    [(eq? key (car (car lst))) (append (list (cons key value)) (cdr lst))]
    [else (append (list (car lst)) (update key value (cdr lst)))]))


(define (make-object name)
  (begin (define (modify key lst fn)
           (let rec([k key]
                    [l lst]
                    [fn fn]
                    [acc '()])
             (if (equal? k (car (first l)))
                 (append (append acc (rest l))
                         (list (cons (car (first l)) (my-curry fn (cdr (first l))))))
                 (rec k (rest l) fn (cons (first l) acc)))))
         (let ([attr (list (cons
                            2 name))])
           (lambda (cmd . key)
             (case cmd
               ['get (case (first key)
                       ['name (lookup 2 attr)]
                       [else (lookup (first key) attr)])]
               ['set (case (first key)
                       ['name (set! attr (update 2 (second key) attr))]
                       [else (set! attr (update (first key) (second key) attr))])]

               ['update (set! attr (modify (first key) attr (second key)))]
        ['print (println attr)])))))


;;;;; Part 2: Symbolic differentiation (no automated tests!)

(define (diff var exp)
  (begin 
    (define (check exp)
      (cond [(empty? exp) '()]
            [(equal? (number? exp) #t) 0]
            [(equal? (symbol? exp) #t) (if (equal? var exp)
                                           1
                                           0)]))
    (let ([acc '()])
      (cond [(equal? (list? exp) #f) (check exp)]
            [(equal? (list? exp) #t)
             (cond [(equal? (first exp) '+)
                    (append (cons + '()) (append  (deep-map (my-curry diff var) (rest exp))))]
                   [(equal? (first exp) '*)
                    `(+ (* ,(second exp) ,(diff var (third exp))) (* ,(third exp) ,(diff var (second exp))))]
                   [(equal? (first exp) '^)
                    `(* ,(third exp) (^ ,(second exp) ,(sub1 (third exp))))]
                   [else void])]
            [else (check exp)]))))


;;;;; Part 3: Meta-circular evaluator

(define (my-eval rexp)
  (let my-eval-env ([rexp rexp]
                    [env '()])           ; environment (assoc list)
    (cond [(symbol? rexp)                ; variable
           (lookup rexp env)]
          [(eq? (first rexp) 'lambda)    ; lambda expression
           (lambda (v) (my-eval-env (third rexp) (update (car (second rexp)) v env)))]
          [else                          ; function application
           (apply (my-eval-env (first rexp) env) (list (my-eval-env (second rexp) env)))])))

;;;;; Part 4: Free variables

(define (free sexp)
  (let freeenv ([fenv '()])
    (let my-free-env ([rexp sexp]
                      [env '()])            ; environment (assoc list)  
      (cond [(symbol? rexp)                ; variable
             ;; if we find out a variable is free, add it to the free variable list
             (if (lookup rexp env)         
                 (void)
                 (set! fenv (cons rexp fenv)))]
            [(eq? (first rexp) 'lambda)    ; lambda expression
             ;; Bind second to the existing env, and evaluate the function body 
             (my-free-env (third rexp) (update (car (second rexp)) (car (second rexp)) env))
             ]
            [(symbol? (first rexp))                    ; function application with symbol case
             (my-free-env (first rexp) env)            ; we check if the symbol is free or not
             (my-free-env (second rexp) env)           ; we check if the applied value is free or not
             ]
            [(eq? (first (first rexp)) 'lambda)        ; function application with lambda case, we bind (key.value) in lambda function and check free
             (my-free-env (third (first rexp)) (update (car (second (first rexp))) (my-free-env (second rexp) env) env))
             ]) )
    ((lambda (v) fenv) fenv)))



;;;;; Extra credit: algebraic simplification (add your own tests)

;; Implemented features:
;; - ...
(define (simplify exp)
  (void))
