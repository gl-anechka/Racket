; Список квадратов первых n чисел Фибоначчи, линейно-итерационная версия
; без функций высшего порядка

#lang racket
(define (list-fib-squares-1 n)
  (define (loop i prev cur lst)
    (if (= i 0) (reverse lst)
        (loop (- i 1) cur (+ prev cur) (cons (* cur cur) lst))))
  (loop n 0 1 '()))

(list-fib-squares-1 1000)