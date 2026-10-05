; Список квадратов первых n чисел Фибоначчи, линейно-итерационная версия
; со сверткой

#lang racket
(define (list-fib-squares-2 n)
  (define (loop i prev cur lst)
    (if (= i 0) (reverse lst)
        (loop (- i 1) cur (+ prev cur) (cons cur lst))))
  (loop n 0 1 '()))

(foldl (lambda (a lst) (cons (* a a) lst)) '() (reverse (list-fib-squares-2 1000)))