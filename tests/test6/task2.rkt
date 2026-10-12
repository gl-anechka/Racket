; Функция, возвращающая n-й элемент последовательности нечётных недостаточных чисел.
; Недостаточным является любое натуральное число X, у которого сумма всех его
; натуральных делителей меньше 2*X.

#lang racket
(require math/number-theory)

; сумма делителей числа n
(define (sum-of-div n)
  (let loop ((d 1) (sum 0))
    (cond
      ((> (* d d) n) sum) ; смотрим делители до корня из числа
      ((divides? d n) ; можем добавить делители парами
       (let ((d2 (quotient n d)))
         (loop (+ d 2) (+ sum d (if (= d2 d) 0 d2))) ; на случай совпадения делителей
       )
      )
      (else (loop (+ d 2) sum)) ; можем идти в шагом 2, т.к. четных делителей у нечетного числа быть не может
    )
  )
)

; проверка на недостаточность
(define (deficient? n)
  (< (sum-of-div n) (* 2 n))
)

; найти первое нечетное недостаточное число, начиная с текущего
(define (first-odd-deficient n)
  (if (deficient? n)
      n
      (first-odd-deficient (+ n 2))
  )
)

; хеш-таблица для хранения посчитанных результатов
(define deficient-table (make-hash '((1 . 1))))

; без мемоизации
(define (odd-deficient n)
  (let loop ((i (- n 1)) (cur 1))
    (if (= i 0) cur
        (loop (sub1 i) (first-odd-deficient (+ cur 2)))
    )
  )
)

; с мемоизацией
(define (memo-odd-deficient n)
  (let ((res (hash-ref deficient-table n #f))) ; проверка присутствия значения в таблице
    (if res res
        (let ((val (first-odd-deficient (+ (memo-odd-deficient (sub1 n)) 2))))
          (hash-set! deficient-table n val) ; добавляем посчитанное значение в таблицу
          val
        )
    )
  )
)


; Ответ на вопрос:
; Мемоизация выгодна при многократных запросах, когда индексы повторяются.
; При однократном вычислении она не даёт выигрыша.