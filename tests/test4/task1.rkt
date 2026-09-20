; Функция task-4 проверяет, является ли двоичное дерево tree совершенным деревом
; пустое дерево - совершенное дерево высоты 0
; дерево из одного листа - совершенное высоты 1
; дерево, левый и правый листы которого совершенные деревья высоты 1 - совершенное высоты 2
; и т.д.

#lang racket

; функции с лекции
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (task-4 tree)
  ; если совершенное, вернуть его высоту, иначе #f
  (define (check-perfect tree)
    (if (empty-tree? tree)
        0
         (let
            (
             (height1 (check-perfect (tree-left tree)))
             (height2 (check-perfect (tree-right tree)))
            )

            ; оба поддерева совершенные и их высоты совпадают
            (if (and
                 height1
                 height2
                 (= height1 height2)
                 )
                (+ height1 1)
                #f
             )
          )
      )
     )
  (if (check-perfect tree) #t #f)
  )