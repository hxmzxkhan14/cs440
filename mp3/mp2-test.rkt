#lang racket

(require rackunit
         "mp2.rkt")


(test-case "boolean literals"
           (check-equal? (eval (parse #t)) #t)
           (check-equal? (eval (parse #f)) #f)
           (check-equal? (eval (parse '(let ([x #t]) x))) #t))


(test-case "basic if expressions"
           (check-equal? (eval (parse '(if #t 1 2))) 1)
           (check-equal? (eval (parse '(if #f 1 2))) 2)
           (check-equal? (eval (parse '(if #t (+ 1 2) 2))) 3)
           (check-equal? (eval (parse '(let ([x #t] [y 1] [ z 2])
                                         (if x y z))))
                         1))


(test-case "relational expressions"
           (check-equal? (eval (parse '(= 1 1))) #t)
           (check-equal? (eval (parse '(= 1 2))) #f)
           (check-equal? (eval (parse '(< 1 2))) #t)
           (check-equal? (eval (parse '(< 2 1))) #f)
           (check-equal? (eval (parse '(if (= 1 1) 1 2))) 1)
           (check-equal? (eval (parse '(< (+ 1 2) (* 2 3)))) #t)
           (check-equal? (eval (parse '(let ([x 1] [y (+ 3 4)])
                                         (if (< x y) y x))))
                         7)
           (define sexp '((lambda (x y)
                            (if (< x y) (+ x y) (* x y)))))
           (check-equal? (eval (desugar (parse (append sexp '(1 2))))) 3)
           (check-equal? (eval (desugar (parse (append sexp '(3 2))))) 6))


(test-case "boolean expressions"
           (check-equal? (eval (desugar (parse '(and #t)))) #t)
           (check-equal? (eval (desugar (parse '(and #f)))) #f)
           (check-equal? (eval (desugar (parse '(or #t)))) #t)
           (check-equal? (eval (desugar (parse '(or #f)))) #f)
           (check-equal? (eval (desugar (parse '(and #t #t #t #t #t)))) #t)
           (check-equal? (eval (desugar (parse '(or #f #f #t #f #f)))) #t)
           (check-equal? (eval (desugar (parse '(and #t #t #t #t #f)))) #f)
           (check-equal? (eval (desugar (parse '(and #t #t #t #t #f)))) #f)
           (check-equal? (eval (desugar (parse '(and (< 1 2) (= 2 (+ 1 1)))))) #t)
           (check-equal? (eval (desugar (parse '(or (< 3 2) (= 3 (+ 1 1)))))) #f))


(test-case "subtraction"
           (check-equal? (eval (desugar (parse '(- 2 3)))) -1)
           (check-equal? (eval (desugar (parse '(- (* 2 5) (+ 1 3))))) 6)
           (check-not-equal? (parse '(- 1 2)) (desugar (parse '(- 1 2)))))


(test-case "cond expression"
           (check-equal? (eval (desugar (parse '(cond [#t 1] [#t 2] [else 3])))) 1)
           (check-equal? (eval (desugar (parse '(cond [#f 1] [#t 2] [else 3])))) 2)
           (check-equal? (eval (desugar (parse '(cond [#f 1] [#f 2] [else 3])))) 3)
           (check-equal? (eval (desugar (parse '(let ([x 1] [y 2] [z (+ 3 4)])
                                                  (cond [(< x y) x]
                                                        [(= x y) y]
                                                        [(< y z) (+ x z)]
                                                        [else #t])))))
                         1)
           (check-equal? (eval (desugar (parse '(let ([x (+ 1 1)] [y 2] [z (+ 3 4)])
                                                  (cond [(< x y) x]
                                                        [(= x y) y]
                                                        [(< y z) (+ x z)]
                                                        [else #t])))))
                         2)
           (check-equal? (eval (desugar (parse '(let ([x 4] [y 2] [z (+ 3 4)])
                                                  (cond [(< x y) x]
                                                        [(= x y) y]
                                                        [(< y z) (+ x z)]
                                                        [else #t])))))
                         11)
           (check-equal? (eval (desugar (parse '(let ([x 10] [y 8] [z (+ 3 4)])
                                                  (cond [(< x y) x]
                                                        [(= x y) y]
                                                        [(< y z) (+ x z)]
                                                        [else #t])))))
                         #t)
           (check-not-equal? (parse '(cond [#t 1] [else 2]))
                             (desugar (parse '(cond [#t 1] [else 2])))))


(test-case "relational expressions (sugar)"
           (check-equal? (eval (desugar (parse '(> 2 1)))) #t)
           (check-equal? (eval (desugar (parse '(> 2 (* 3 2))))) #f)
           (check-equal? (eval (desugar (parse '(<= 2 (+ 1 1))))) #t)
           (check-equal? (eval (desugar (parse '(<= 2 (+ 2 3))))) #t)
           (check-equal? (eval (desugar (parse '(>= 2 (+ 1 1))))) #t)
           (check-equal? (eval (desugar (parse '(>= 2 (+ 0 1))))) #t)
           (check-not-equal? (parse '(<= 1 2)) (desugar (parse '(<= 1 2))))
           (check-not-equal? (parse '(> 1 2))  (desugar (parse '(> 1 2))))
           (check-not-equal? (parse '(>= 1 2)) (desugar (parse '(>= 1 2)))))


(test-case "define"
           (define test1-defs (load-defs "test1.defs"))
           (define test2-defs (load-defs "test2.defs"))
           (check-equal? (length test1-defs) 3)
           (check-equal? (eval (desugar (parse '(fn-a 1)))
                               test1-defs)
                         11)
           (check-equal? (eval (desugar (parse '(fn-b 2)))
                               test1-defs)
                         40)
           (check-equal? (eval (desugar (parse '(fn-c 3)))
                               test1-defs)
                         70)
           (check-equal? (eval (desugar (parse '(sum-to 10)))
                               test2-defs)
                         55)
           (check-equal? (eval (desugar (parse '(fib 10)))
                               test2-defs)
                         55)
           (check-equal? (eval (desugar (parse '(even 10)))
                               test2-defs)
                         #t)
           (check-equal? (eval (desugar (parse '(odd 10)))
                               test2-defs)
                         #f)
           (check-equal? (eval (desugar (parse '((make-adder 99) 100)))
                               test2-defs)
                         199))
