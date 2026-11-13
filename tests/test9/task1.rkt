#lang racket
(require racklog)

; расставить N ладей на шахматной доске N*N,
; чтобы они не били друг друга

; список свободных столбцов и расстановка
(define %solution
  (%rel (rooks n free)  ; локальные переменные
   [('() 0)]
   [(rooks n)  ; шаблон
    (%> n 0)
    (%is free (range 1 (add1 n)))
    (%find-place rooks n free)
   ]
  )
)

; расстановка фигур по строкам
; free - свободные столбцы, rooks - список расставленных фигур
(define %find-place
  (%rel (row free col rest remaining next)
    [('() 0 (_))]
    [((cons (cons row col) rest) row free)
     (%member col free)  ; выбираем свободный столбец
     (%is remaining (remove col free))  ; удаляем выбранный столбец
     (%is next (sub1 row))  ; переходим к предыдущей строке
     (%find-place rest next remaining)  ; расставляем остальные ладьи
    ]
  )
)