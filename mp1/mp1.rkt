#lang racket

(provide (all-defined-out)) ; export all top-level definitions for testing

(require racket/trace)


;; 1. Integer exponentiation

#; (define (iexpt n e)
  (trace-let rec ([acc 1]
                  [val n])
                  (if (= acc e)
                      val
                      (rec (add1 acc) (* val n)))))

(define (iexpt n e)
  (let rec ([acc 1]
                  [val n])
             (cond
               [(< e 1) 1]
               [(if (= acc e)
                    val
                    (rec (add1 acc) (* val n)))])))

(trace iexpt)


;; 2. Polynomial evaluation

(define (poly-eval coeffs x)
  (let rec ([lst coeffs]
            [int x]
            [n 0]
            [acc 0])
    (cond
      [(null? lst) acc]
      [else (rec (rest lst)
              int
              (+ n 1)
              (+ acc (* (first lst) (expt int n))))])))


(trace poly-eval)


;; 3. List concatenation

; helper function (concat)

(define (concat lst1 lst2)
  (let rec ([l (reverse lst1)]
            [acc lst2])
    (if (empty? l)
        acc
        (rec (rest l) (cons (first l) acc)))))

(define (concatenate . lsts)
  (let rec ([result '()]
            [l (reverse lsts)])
    (if (empty? l)
        result
        (rec (concat (first l) result)
          (rest l)))))

(trace concatenate)


;; 4. List ordered merging (non-tail-recursive)

(define (merge l1 l2)
  (trace-let rec ([final (concat l1 l2)])
             ((lambda (x)
                (sort x <)) final)))

(trace merge)

;; 5. List ordered merging (tail-recursive)

(define (merge-tail l1 l2)
  (trace-let rec ([final (concat l1 l2)])
             ((lambda (x)
                (sort x <)) final)))

(trace merge-tail)

;; 6. List run-length encoding

#; (define (run-length-encode1 lst)
  (trace-let rec ([result '()]
                  [acc 0])
             (cond
               [(equal? (car lst) (car (cdr lst)))
                (rec (car lst) (add1 acc))]
               [else
                (rec ((car (cdr lst)) 0))])))

(define (run-length-encode lst)
  (trace-let rec ([copy lst]
                  [acc 1]
                  [result `(,(list (car lst) 1))])
             (trace-let func ([result `(,(list (car lst) acc))])                        
             (cond
               [(equal? (car lst) (second lst))
                (rec (rest copy) (add1 acc) (cons (list (car copy) acc) result))]
               [else
                (rec (rest copy) 1 (cons (list (car copy) acc) result))]))))
                

;; 7. List run-length decoding

(define (run-length-decode lst)
  (trace-let rec ([result '()]
                  [fir (car lst)]
                  [acc 0]
                  [len (length lst)])
             (if (= acc len)
                 result
                 (rec (helper1 fir) (car (rest lst)) (add1 acc) len))))

; helper function

(define (helper1 lst)
  (trace-let rec ([val (car lst)]
                  [lim (car (reverse lst))])
             (trace-let func ([acc 0]
                              [result '()])                               
                        (if (= acc lim)
                            result
                            (func (add1 acc) (cons val result))))))

#;(define (run-length-decode lst)
  (trace-let rec ([result '()])
             (cond
               [(null? lst) result]
               [(< (cdr (car lst)) 1)
                (run-length-decode (rest lst))]
               [else (rec (cons (car (car lst))))
                     (sub1 (cdr (car lst)))])))
                 

#;(define (run-length-decode2 lst)
  (trace-let rec ([result '()])
             (if (null? lst)
                 result
                  ((sub1 (car (reverse (car lst))))
                  (rec (car (car lst)))
                  (when (= (car (reverse (car lst))) 0)
                    (run-length-decode2 (rest lst)))))))

;; 8. Labeling sexps

(define (label-sexp sexp)
    (match sexp
      [(? integer?)
       `(int ,sexp)]

      [(? symbol?)
       `(var ,sexp)]

      [(list (and op (or '+ '* '/ '-)) lhs rhs)
       `(arith (op ,op) ,lhs ,rhs)]


      [_ (error "Can't label!")]))