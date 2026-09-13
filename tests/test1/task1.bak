; Проверка противоположной направленности векторов 3-х мерного пр-ва

#lang racket
; проверка на пропорциональность двух координат
(define (checkprop a b c d)
  (= (* a d) (* b c)))

(define (opposite? x1 y1 z1 x2 y2 z2)
  (and (checkprop x1 x2 y1 y2) (checkprop y1 y2 z1 z2) (checkprop x1 z1 x2 z2)
       (< (+ (* x1 x2) (* y1 y2) (* z1 z2)) 0)))