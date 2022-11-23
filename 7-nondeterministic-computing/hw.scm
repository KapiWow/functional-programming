(load "../lib/nc.scm")

;Task 1
;2 балла
;Добавьте реализацию let.

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ;((let? exp) (let->combination exp))
        ((amb? exp) (analyze-amb exp))
        ((require? exp) (analyze-require exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))


(define prog '(
(define (odd-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (odd? (+ a b)))
    (+ a b)))
))
(run-prog prog)

;Раскомментируйте тест
;(define prog '(
;(odd-sum-pair (list 1 3 5 8) (list 20 35 110))
;))
;(run-prog prog)


;===================

;Бейкер, Купер, Флетчер, Миллер и Смит живут на разных этажах многоквартирного
;дома, в котором всего пять этажей. Бейкер не живет на верхнем этаже. Купер не
;живет на нижнем этаже. Флетчер не живет ни на верхнем, ни на нижнем этаже.
;Миллер живет на более высоком этаже, чем Купер. Смит не живет на этаже,
;примыкающем к дому Флетчера. Флетчер не живет на этаже рядом с Купером. Где все
;живут?

(define prog '(
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
))
(run-prog prog)

(define prog '(
(define (multiple-dwelling)
    (define baker (amb 1 2 3 4 5)) 
    (define cooper (amb 1 2 3 4 5))
    (define fletcher (amb 1 2 3 4 5)) 
    (define miller (amb 1 2 3 4 5))
    (define smith (amb 1 2 3 4 5))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker) (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith)))
))

(run-prog prog)

;Изначальная версия
(define prog '(
(multiple-dwelling)
))
(run-prog prog)
;(get-exec-time (lambda () (repeat (lambda() (run-prog prog)) 10)))

; Task 2
; 2 балла
; модифицируйте программу, чтобы она выполнялась хотя бы в (5?) раз быстрее

;;(define prog '(
;;(define (multiple-dwelling) 
;;???
;;))

;(run-prog prog)

;(define prog '(
;(multiple-dwelling)
;))

;Ваша версия, которая будет в (5?) раз быстрее
;(get-exec-time (lambda () (repeat (lambda() (run-prog prog)) 10)))

; Task 3
; 2 балла
; Решите задачу, написав программу

;Пять школьниц сдали экзамен. Их родители, как они думали, проявляли чрезмерный
;интерес к результату. Поэтому они согласились, что в письме домой об
;обследовании каждая девушка должна сделать одно верное утверждение и одно
;ложное. Ниже приведены соответствующие отрывки из их писем:

;* Бетти: «Китти была второй на экзамене. Я была только третьей».
;* Этель: «Вы будете рады услышать, что я была на высоте. Джоан была второй».
;* Джоан: «Я была третьей, а Этель была последней».
;* Китти: «Я вышла второй. Мэри была только четвертой».
;* Мэри: «Я была четвертой. Первое место заняла Бетти».
