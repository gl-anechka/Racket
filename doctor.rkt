; Практическое задание "Доктор". Сентябрь 2026
#lang scheme/base

; Подключаем Racket-библиотеки для векторов и списков, на всякий случай
(require racket/vector)
(require racket/list)

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name #()) ; изначально история реплик пациента пустая
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
;(define (doctor-driver-loop name)
;    (newline)
;    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
;    (let ((user-response (read)))
;      (cond 
;	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
;             (printf "Goodbye, ~a!\n" name)
;             (print '(see you next week)))
;            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
;                  (doctor-driver-loop name)
;             )
;       )
;      )
;)

; диалог с Доктором, где Доктор запоминает фразы пациента
; параметр name - имя пациента, параметр history - вектор сохраненных фраз пациента
(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  ; добавляем реплику в исторический вектор (ф-ия добавления описана ниже)
                  ; после ответа Доктора, чтобы выбирать ответ только из прошлых фраз
                  (doctor-driver-loop-v2 name (add-to-history user-response history))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
;(define (reply user-response)
;      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
;          ((0) (hedge-answer))  ; 1й способ
;          ((1) (qualifier-answer user-response)) ; 2й способ
;      )
;)

; генерация ответной реплики по user-response -- реплике от пользователя и исторического вектора history
(define (reply user-response history)
  (if (history-applicable? history) ; проверяем, можно ли применять третью стратегию ответа, т.е. есть ли в нем неиспользованные реплики
      (case (random 0 3) ; с равной вероятностью выбирается один из трех способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ
      )
      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
      )
  )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector #((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (tell me more)
                              (keep going - i am listening)
                              (go at your own pace)
                              (a lot of people feel that way)
                              (you are not alone in this))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector #((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (do you often think that)
                                       (deep down you believe that)
                                       (perhaps you believe that)
                                       (from your perspective))
                )
                (change-person user-response)
        )
 )

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

; версия many-replace с хвостовой рекурсией, используется именованный let
; предыдущая версия имела рекурсивный вызов, так как после завершения вызова работает cons (это остаточные вычисления)
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((rest lst) (result '())) ; храним еще не обработанную часть ответа и результирующую
    (if (null? rest) (reverse result) ; поскольку добавление в начало, то ответ перевернут
        (let ((pat-rep (assoc (car rest) replacement-pairs))) ; та же самая логика обработки элемента списка
          (loop (cdr rest)                                   ; только теперь вызывается loop
                (cons (if pat-rep (cadr pat-rep)
                          (car rest)
                      )
                      result
                )
          )
        )
    )
  )
)

; версия many-replace с функциями высшего порядка, тело состоит только из вызова map, функция обработки задать как анонимную
(define (many-replace-v3 replacement-pairs lst)
  (map ; проходится по списку и для каждого слова применяет функцию ниже, список строится в том же порядке
   (lambda (x) ; каждый раз для обработки берем текущее слово
     (let ((pat-rep (assoc x replacement-pairs))) ; ищем в ассоциативном списке замену
       (if pat-rep (cadr pat-rep) ; если поиск удачный (pat-rep вернул не #f), берем замену
           x ; иначе оставляем текущее слово
       )
      )
   )
   lst ; обрабатываемый список
  )
)

; 3я стратегия ответа - выбор ответа из исторического вектора
(define (history-answer history)
  (let* ((unused (vector-filter  ; выбираем неиспользованные реплики, результат пишем в новый вектор unused
                  (lambda (lst) (not (cadr lst))) ; берем только реплики с флагом #f
                  history))
         (chosen (pick-random-vector unused)) ; случайно выбранная реплика, которую надо пометить в историческом векторе как использованную
         (ind (vector-member chosen history)) ; индекс случайно выбранной реплики
        )
     (vector-set! history ind (list (car chosen) #t)) ; меняет элемент вектора по индексу (в нашем случае поменять флаг использованной фразы)
    (append '(earlier you said that) (change-person (car chosen))) ; итоговая фраза Доктора с отсылкой на ранее сказанную реплику
  )
)

; проверка применимости третьей стратегии ответа, возвращает не #f, если есть хотя бы одна фраза с флагом #f
(define (history-applicable? history)
  (vector-member
     #f ; ищем флаг, что фраза не применялась
     history ; вектор, в котором ищем
     (lambda (flag hist) (equal? flag (cadr hist))) ; функция сравнения, берет только флаг из элемента вектора
  )
)

; проверка присутствия реплики в историческом векторе, позволяет хранить только уникальные реплики
(define (is-in-history? user-response history)
  (vector-member ; ищет элемент в векторе и возвращает индекс первого найденного элемента или #f
     user-response
     history ; исторический вектор хранит реплику и флаг использования
     (lambda (user-rep hist) (equal? user-rep (car hist))) ; функция сравнения берет только реплику (без флага) для сравнения
  )
)

; функция добавления новой реплики пациента в исторический вектор,
; проверяет присутсвие реплики в векторе и в случае #f добавляет её в вектор
(define (add-to-history user-response history)
  (if (is-in-history? user-response history) ; проверка присутствия через ранее написанную функцию
      history ; реплика уже есть в векторе
      (vector-append (vector (list user-response #f)) history) ; создаем элемент вектора (реплика #f) и добавляем в голову исторического вектора
  )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))
