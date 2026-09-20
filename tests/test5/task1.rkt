#lang racket

; функции с лекции
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


; Функция получает двоичное дерево в векторном представлении
; и проверяет, что функция fun-p не даёт ложь ни на одном значении
; при всех вершинах дерева
(define (for-all-tree fun-p tree)
  (define (check-cps tree cc)
    (cond
      ;в пустой вершине проверять нечего
      ((empty-tree? tree) (cc #t))
      ;сама вершина не удовлетворяет условию -> дальше смотреть бессмысленно
      ((not (fun-p (tree-data tree))) (cc #f))
      ;проверка левого и правого поддерева
      (else (check-cps
             (tree-left tree)
             (lambda (x)
               (if x
                   (check-cps (tree-right tree) cc)
                   (cc #f)
                )
              )
             )
       )
     )
   )
  (check-cps tree (lambda (x) x))
 )