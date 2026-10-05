; Список квадратов первых n чисел Фибоначчи

#lang racket
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))) )))

(define (accumulate combiner null-val term a next b)
  (let loop ((a a) (result null-val))
    (if (> a b) result
        (loop (next a) (combiner (term a) result))))
)

(define (enumerate-interval a b)
  (accumulate (lambda (x y) (append y x))
              '() list a add1 b))

(define (list-fib-squares n)
  (map (lambda (x)
         (let ((temp (fib x))) (* temp temp)))
       (enumerate-interval 1 n)))

(list-fib-squares 10)