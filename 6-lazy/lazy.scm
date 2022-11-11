(load "../lib/utils.scm")
(load "../lib/slides.scm")
(load "../lib/meta.scm")

(next-slide "Порядок выполнения")

(define (try a b) (if (= a 0) 1 b))

;(try 0 (/ 1 0))
;значение не используется, но будет ошибка

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define b 0)
(define a 5)

;(unless (= b 0)
;  (/ a b)
;  (begin (display "exception: returning 0") 0))
; Тоже будет ошибка
;Это связано с порядком вычисления выражений.  Scheme использует
;applicative-order, т.е. вычисляет все аргументы перед вызовом функции

(next-slide "unless factorial")

(define (factorial n)
  (println n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; Можно ли вызвать эту функцию, будет ли она работать?
;(factorial 5)

(next-slide "Модифицирование лиспа")
; старое
;((application? exp)
; (apply (eval (operator exp) env)
;        (list-of-values (operands exp) env)))

; новое
;((application? exp)
; (apply (actual-value (operator exp) env)
;        (operands exp) env))

(define (actual-value exp env)
  (force-it (eval exp env)))

(next-slide "Модифицирование лиспа")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp) env))
        (else
          (error "Unknown expression type: EVAL" exp))))

(next-slide "Новый apply")

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)
           ;arguments
           ))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             ;arguments
             (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))
;argumets is old version

(next-slide "list-of-arg-value")

;задерживаем вычислени с помощью actual-value
;для примитивных процедур
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps)
                        env)
          (list-of-arg-values (rest-operands exps)
                              env))))

(next-slide "list-of-delayed-args")
;задерживаем вычисления с помощью delay-it
;для составных процедур
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps)
                    env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

(next-slide "замена if")

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;старое определение
;(define (eval-if exp env)
;  (if (true? (eval (if-predicate exp) env))
;    (eval (if-consequent exp) env)
;    (eval (if-alternative exp) env)))


(next-slide "driver loop")

(define input-prompt
  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input 'q)
      (println "exit from driver loop")
      (let ((output
              (actual-value ;eval
                input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)
        (driver-loop)))))


(next-slide "force it")

(define (force-it obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj))


(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(next-slide "try again")
;(define (try a b) (if (= a 0) 1 b))
;(try 0 (/ 1 0))

;uncomment for test
(define the-global-environment (setup-environment))
;(driver-loop)

(next-slide "memorization")

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj)
                     result)
           ; replace exp with its value
           (set-cdr! (cdr obj)
                     '())
           ; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))


(next-slide "Пример")

(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))
;;;; L-Eval input:
;count
;;;; L-Eval value:
;;???
;;;; L-Eval input:
;w
;;;; L-Eval value:
;;???
;;;; L-Eval input:
;count
;;;; L-Eval value:
;;???
;count
;;;; L-Eval value:
;;???

(next-slide "Пример")
;;;; L-Eval input:
;count
;;;; L-Eval value:
;;1
;;;; L-Eval input:
;w
;;;; L-Eval value:
;;10
;;;; L-Eval input:
;count
;;;; L-Eval value:
;;2
;count
;;;; L-Eval value:
;;2

;А что если мы не будем использовать memorization?

(next-slide "Запуск без инпута")

(define (run-and-print input env) 
      (let ((output
              (actual-value
                input the-global-environment)))
            (if (not (eq? output 'ok))
            (begin (user-print output) (println "") )
            )))

(define (run-prog prog) 
    (map (lambda (x) (run-and-print x env)) prog))

(next-slide "Запуск примера")

(define env (setup-environment))

(define prog '(
    (define count 0)
    (define (id x) (set! count (+ count 1)) x)
    (define w (id (id 10)))
    count
    w
    count
))

(run-prog prog)

(next-slide "Ленивые списки")

(define prog '(
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))


(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
))

(run-prog prog)

(next-slide "Ленивые списки: Пример")

(define prog '(
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

(list-ref integers 17)
))

(run-prog prog)
