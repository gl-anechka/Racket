#lang racket

; принимает список чисел и возвращает список номеров минимального элемента списка
(define (taskI lst)
  (if (null? lst)
      '()
      (let ((result
             (foldl (lambda (x state)
                      (let ((xmin (car state))
                            (indices (cadr state))
                            (ind (caddr state))
                            )

                        (cond
                          ((< x xmin) (list x (list ind) (add1 ind)))
                          ((= x xmin) (list xmin (cons ind indices) (add1 ind)))
                          (else (list xmin indices (add1 ind)))
                          )
                        )
                      )
                    ;храним минимум, список индексов, индекс текущего элемента
                    (list (car lst) (list 0) 1)
                    (cdr lst)
                    )
             )
            )
        (cadr result)
        )
      )
  )


; находит суммарную площадь чёрных участков изображения
; на вход получает дерево и общую площадь изображения
; дерево - квадродерево, на каждом ярусе значение обозначает цвет "пикселя"
(define (taskII t s)
  (cond
    ((equal? t 0) 0)
    ((equal? t 1) s)
    (else (let ((snew (/ s 4)))
            (+ (taskII (vector-ref t 0) snew)
               (taskII (vector-ref t 1) snew)
               (taskII (vector-ref t 2) snew)
               (taskII (vector-ref t 3) snew)
               )
            )
          )
    )
  )


; возвращает #t, если суммарная площадь черных участков квадродерева t
; больше, чем суммарная площадь его белых участков
; надо сделать эффективную версию
(define (taskIII t)
  (call/cc
   (lambda (cc-exit)
     (define white 0)
     (define black 0)
     
     (define (check t s)
       (cond
         ;белый
         ((equal? t 0)
          (set! white (+ white s))
          (if (>= white 1/2) (cc-exit #f) #t)
          )
         ;черный
         ((equal? t 1)
          (set! black (+ black s))
          (if (> black 1/2) (cc-exit #t) #f)
          )
         (else (let ((snew (/ s 4)))
                 (check (vector-ref t 0) snew)
                 (check (vector-ref t 1) snew)
                 (check (vector-ref t 2) snew)
                 (check (vector-ref t 3) snew)
                 )
               )
         )
       )

     ;не умаляя общности примем площадь за 1
     (check t 1)
     )
   )
  )


; реализует (taskII t s), но составлена в стиле передачи продолжений
(define (taskIV-сс t s cc)
  (cond
    ((equal? t 0) (cc 0))
    ((equal? t 1) (cc s))
    (else (let ((snew (/ s 4)))
            (+ (taskIV-сс
                (vector-ref t 0)
                snew
                (lambda (x)
                  (taskIV-сс
                   (vector-ref t 1)
                   snew
                   (lambda (y)
                     (taskIV-сс
                      (vector-ref t 2)
                      snew
                      (lambda (z)
                        (taskIV-сс
                         (vector-ref t 3)
                         snew
                         (lambda (w)
                           (cc (+ x y z w)))
                         )
                        )
                      )
                     )
                   )
                  )
                )
               )
            )
          )
    )
  )


; вычислить f1(f2(...fn(x)))
; по идее через . передается произвольное кол-во аргументов
(define (taskV . args)
  ;надо вернуть новую функцию одного аргумента
  (lambda (x) (foldr (lambda (f arg) (f arg)) x args)
   )
 )