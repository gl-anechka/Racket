; Функция task-5 проверяет, является ли двоичное дерево
; совершенным деревом высоты h

#lang racket
; функции с лекции
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (task-5 tree h)
  (call/cc
   (lambda (cc-exit)
     (define (check-perfect tree h)
       (cond
         ;пустое дерево, нулевая глубина
         ((= h 0)
          (if (empty-tree? tree)
              #t
              (cc-exit #f)
              )
          )

         ;пустое дерево, ненулевая глубина
         ((empty-tree? tree) (cc-exit #f))

         ;проверка правого и левого поддерева
         (else
          (check-perfect (tree-left tree) (- h 1))
          (check-perfect (tree-right tree) (- h 1))
          )
         )
       )
     (check-perfect tree h)
     )
   )
  )