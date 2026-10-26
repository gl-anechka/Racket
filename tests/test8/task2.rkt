; С помощью оператора Маккарти amb опишите функцию (task-amb tree),
; принимающую непустое дерево.
; Функция task-amb возвращает кратчайший из всех имеющихся в дереве путей,
; таких что: они начинаются в корне дерева, в них не повторяются дуги и
; вершины; заканчивается путь в вершине, где стоит тоже число, что и в корне,
; и больше такое число не встречается ни в одной промежуточной вершине пути;
; путь содержит не менее, чем одну дугу.

#lang racket
(require racket/control)
(require data/queue)

; в этой задаче удобен поиск в ширину, т.к. хотим найти кратчайший маршрут

; реализацию amb возьмем из лекции
(define failures (make-queue)) ; стек возвратов (на базе очереди)

(define (fail) ; обработка возврата
  (if (queue-empty? failures) '()
      ((dequeue! failures)))) ; достаём 1ую альтернативу и вычисляем

(define (amb/thunks choices) ; аргумент – список альтернатив-санков
  (call/cc (lambda (k) ; запоминаем точку
             (map (lambda (choice)
                    (enqueue! failures (lambda () (call-in-continuation k choice))))
                  choices) ; записываем все альтернативы в failures
             (fail)))) ; вынуждаем возврат

; нужен макрос, чтобы альтернативы не вычислять, а превратить в санки
(define-syntax amb
  (syntax-rules ()
    ((amb) (fail))
    ((amb exp ...) (amb/thunks (list (lambda () exp) ...)))))


; ----- собственно решение задачи -----

; поиск пути от текущей вершины до первого повторения
; node - текущая вершина, root - корень, path - пройденный путь в обратном порядке
(define (search node root path)
  (let ((next (amb (vector-ref node 1) (vector-ref node 2))))
    (cond
      ((null? next) '())
      ((= (vector-length next) 0) (amb))
      ((= (vector-ref next 0) root) (reverse (cons root path)))
      (else (search next root (cons (vector-ref next 0) path)))
    )
  )
)

; поиск кратчайшего пути
(define (task-amb tree)
  (set! failures (make-queue)) ; очистка очереди перед поиском
  (search tree (vector-ref tree 0) (list (vector-ref tree 0)))
)