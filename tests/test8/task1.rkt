; Считая описанными бесконечные потоки ones, ints, уместно используя подходящие
; • базовые потоковые функции: stream-cons, stream-first, stream-rest, stream;
; • потоковые функции высших порядков: stream-map, stream-filter, stream-fold;
; • функции stream-scale, stream-sum и stream-mul, введённые на лекции,
; опишите бесконечный поток степеней 2 и 3, упорядоченных по возрастанию: 1 2 3 4 8 9 16 27 32 ...

#lang racket
(require racket/stream)

; функции из лекции

; умножает каждый элемент потока на число
(define (stream-scale s f)
  (stream-map (lambda (x) (* x f)) s))

  
; бесконечный поток степени 2
(define 2-power (stream-cons 1 (stream-scale 2-power 2)))

; бесконечный поток степени 3
(define 3-power (stream-cons 1 (stream-scale 3-power 3)))

; слияние двух потоков в один
(define (merge-streams s1 s2)
  (let ((x (stream-first s1))
        (y (stream-first s2)))
    (cond
      ((< x y) (stream-cons x (merge-streams (stream-rest s1) s2)))
      ((< y x) (stream-cons y (merge-streams (stream-rest s2) s1)))
      (else (stream-cons x (merge-streams (stream-rest s1) (stream-rest s2))))
    )
  )
)

; итоговая функция
(define func (merge-streams 2-power 3-power))