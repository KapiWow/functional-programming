(load "../lib/utils.scm")
(load "../lib/meta.scm")

(next-slide "Поиск простой суммы двух списков")

;(define (prime-sum-pair list1 list2)
;  (let ((a (an-element-of list1))
;        (b (an-element-of list2)))
;    (require (prime? (+ a b)))
;    (list a b)))

;Мы хотим написать новый evaluator, который сможет сам выбирать элементы
;;; Amb-Eval input:
;(prime-sum-pair '(1 3 5 8) '(20 35 110))
;;; Starting a new problem
;;; Amb-Eval value:
;(3 20)

(next-slide "amb")

;ambiguously
;(amb ⟨e1⟩ ⟨e2⟩ : : : ⟨en⟩)

;(list (amb 1 2 3) (amb 'a 'b))
;can have six possible values:
;(1 a) (1 b) (2 a) (2 b) (3 a) (3 b)

;(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(next-slide "amb")

;When the evaluator encounters an application of amb, it initially selects the
;first alternative. This selection may itself lead to a further choice. The
;evaluator will always initially choose the first alternative at each choice
;point. If a choice results in a failure, then the evaluator automagically
;backtracks to the most recent choice point and tries the next alternative.  If
;it runs out of alternatives at any choice point, the evaluator will back up to
;the previous choice point and resume from there. This process leads to a search
;strategy known as depth-first search or chronological backtracking.


(next-slide "amb driver loop" )

;;; Amb-Eval input:
;(prime-sum-pair '(1 3 5 8) '(20 35 110))
;;;; Starting a new problem
;;;; Amb-Eval value:
;(3 20)
;;;; Amb-Eval input:
;try-again
;;;; Amb-Eval value:
;(3 110)
;;;; Amb-Eval input:
;try-again
;;;; Amb-Eval value:
;(8 35)
;;;; Amb-Eval input:
;try-again
;;;; There are no more values of
;(prime-sum-pair (quote (1 3 5 8)) (quote (20 35 110)))


(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

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
        ((amb? exp) (analyze-amb exp))
        ((require? exp) (analyze-require exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))


;(lambda (env succeed fail)
;  ;; succeed is (lambda (value fail) : : :)
;  ;; fail is (lambda () : : :)
;  : : :)

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))


(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc
                          (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ; *2*
                            (set-variable-value!
                              var old-value env)
                            (fail2)))))
             fail))))



(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs)
     env
     ;; success continuation for this aproc
     (lambda (arg fail2)
       (get-args
         (cdr aprocs)
         env
         ;; success continuation for
         ;; recursive call to get-args
         (lambda (args fail3)
           (succeed (cons arg args) fail3))
         fail2))
     fail)))



(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
           ((procedure-body proc)
            (extend-environment
              (procedure-parameters proc)
              args
              (procedure-environment proc))
            succeed
            fail))
        (else
          (error "Unknown procedure type: EXECUTE-APPLICATION"
                 proc))))

;Непосредственно здесь происходит магия выбора
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices)
           env
           succeed
           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;require фэйлит, если предикат ложный
(define (require? exp)
  (tagged-list? exp 'require))

(define (require-predicate exp)
  (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                 (fail2) 
                 (succeed 'ok fail2)))
             fail))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline) (display ";;; Starting a new problem ")
          (ambeval
            input
            the-global-environment
            ;; ambeval success
            (lambda (val next-alternative)
              (announce-output output-prompt)
              (user-print val)
              (internal-loop next-alternative))
            ;; ambeval failure
            (lambda ()
              (announce-output
                ";;; There are no more values of")
              (user-print input)
              (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline) (display ";;; There is no current problem")
      (driver-loop))))


(define (run-and-print input) 
  (define (internal-run try-again)
    (if (eq? input 'try-again)
      (try-again)
      (begin
        (newline) (display ";;; Starting a new problem ")
        (ambeval
          input
          the-global-environment
          ;; ambeval success
          (lambda (val next-alternative)
            (announce-output output-prompt)
            (user-print val))
          ;; ambeval failure
          (lambda ()
            (announce-output
              ";;; There are no more values of")
            (user-print input)
            )))))
  (internal-run
    (lambda ()
      (newline) (display ";;; There is no current problem")
      (newline) (display input)
      )))

(define (run-prog prog) 
    (map (lambda (x) (run-and-print x)) prog))

(define prog '(
    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))   
))

(run-prog prog)

(define prog '(
(an-element-of (list 1 2 3 4))
))
(run-prog prog)


(define prog '(
(define (prime-sum-pair list1 list2)
  (define a (an-element-of list1))
  (define b (an-element-of list2))
  (require (odd? (+ a b)))
  (+ a b))
))
(run-prog prog)

(driver-loop)
(newline)
