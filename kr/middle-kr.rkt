#lang scheme
(require racket/vector)
(require math/number-theory)

; 2.I
; правоассоциативная свертка в примитивах
(define (vector-fold-right func init-val vctr)
  (let loop ((i (sub1 (vector-length vctr)))
             (result init-val)
            )
    (if (< i 0) result
        (loop (sub1 i) (func i result (vector-ref vctr i)))
    )
  )
)


; 2.II
; список составных делителей натурального числа
; рекурсивный процесс
(define (fun2a n)
  (define (loop i)
    (if (< i 2) '()
        (let ((result (loop (sub1 i))))
          (if (and (= (remainder n i) 0)
                   (not (prime? i)))
              (cons i result)
              result)
         )
     )
   )
  (loop n)
)

; итеративный
(define (fun2b n)
  (let loop ((i 2)
             (result '()))
    (if (> i n) result
        (loop (add1 i)
         (if (and (= (remainder n i) 0)
                  (not (prime? i)))
             (cons i result)
             result)))))


; 2.III
; праймориал
(define (fun3 n)
  (let loop ((i 0)
             (p 1)
             (result 1)
            )
    (if (= i n) result
        (if (prime? p)
            (loop (add1 i) (add1 p) (* result p))
            (loop i (add1 p) result)
        )
    )
  )
)


; 2.IV
; подсчет отрицательных вершин на заданной глубине
; можно использовать только хвостовую рекурсию
(define (fun4 tree r1 r2)
  (let ((min-r (min r1 r2))
        (max-r (max r1 r2)))
    (let loop ((trees (list (cons tree 0)))
               (result 0))
      (if (null? trees) result
          (let ((t (caar trees))
                (depth (cdar trees))
                (rest (cdr trees)))
            (if (or (vector-empty? t) (> depth max-r))
                (loop rest result)
                (loop
                 (cons (cons (vector-ref t 1) (+ depth 1))
                       (cons (cons (vector-ref t 2) (+ depth 1))
                             rest))
                 (if (and (< (vector-ref t 0) 0) (<= min-r depth max-r))
                     (+ result 1)
                     result)
                 )
                )
            )
          )
      )
    )
)


; 2.V
; проверка на АВЛ-дерево и на совершенное
(define (fun5 tree h)
  (call/cc
   (lambda (сс-exit)
     (define (check t h min-v max-v) ; проверить, что поддерево t должно быть совершенным деревом высоты h,
                                     ; и все его значения должны лежать между min-v и max-v
       (cond
         ((< h 0) (сс-exit #f)) ; ушли ниже требуемой высоты
         ((= h 0) (if (vector-empty? t) #t (сс-exit #f))) ; дерево может быть только пустым при высоте 0
         ((vector-empty? t) (сс-exit #f)) ; дерево закончилось раньше высоты
         (else ; обработка текущей вершины
          (let ((val (vector-ref t 0)))
            (if (or (<= val min-v) (>= val max-v)) ; значение вершины между min и max
                (сс-exit #f)

                (begin
                  (check (vector-ref t 1) (sub1 h) min-v val) ; левое поддерево
                  (check (vector-ref t 2) (sub1 h) val max-v) ; правое поддерево
                  #t) ; обе проверки прошли
             )
           )
          )
        )
     )
     (check tree h -inf.0 +inf.0)
    )
  )
)