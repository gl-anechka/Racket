; Исправление неэффективного списка квадратов первых n чисел Фибоначчи

#lang racket
; линейно-итерационная версия без функций высшего порядка
(define (list-fib-squares-a n)
  (define (loop i prev cur lst)
    (if (= i 0) (reverse lst)
        (loop (- i 1) cur (+ prev cur) (cons (* cur cur) lst))))
  (loop n 0 1 '()))

; линейно-итерационная версия со сверткой
(define (list-fib-squares-b n)
  (define fib-list
    (foldl (lambda (i lst)
             (define prev (car lst))
             (define cur (cadr lst))
             (define other (caddr lst))
             (list cur (+ prev cur) (cons (* cur cur) other)))
           (list 0 1 '())
         (build-list n (lambda (x) x))))
  (reverse (caddr fib-list)))