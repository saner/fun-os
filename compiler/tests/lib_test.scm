(define (test-vector-comp)
  (let ((v (vector 1 (vector 10 20) (+ 2 3))))
    (begin  (print-int (vector-ref v 0))
            (print-int (vector-ref (vector-ref v 1) 0))
            (print-int (vector-ref v 2)))))

(define (test-vector-literal)
  (let ((v (vector 1 2 3 4 5)))
    (begin  (print-int (vector-ref v 0))
            (print-int (vector-ref v 1))
            (print-int (vector-ref v 2))
            (print-int (vector-ref v 3))
            (print-int (vector-ref v 4)))))

(define (test-vector-simp)
  (let ((v (begin (comment "vector construct")
                  (let ((vec01 (make-vector 4)))
                    (begin (vector-set! vec01 0 3) vec01)))))
    (print-int (vector-ref vec01 0))))

(define (test-list-exp-2)
  (let ((ls (list 1 (list 10 20) 3)))
    (print-int (car (cdr (car (cdr ls)))))))

(define (test-list-exp)
  (let ((ls (list 1 2 3)))
    (print-int (cdr (cdr (cdr ls))))))

(define (test-cons-complex)
  (let ((ls (cons 1 (cons 2 3))))
    (print-int (cdr (cdr ls)))))


(define (test-let-2)
  (let ((v1 (make-vector 4)) (v2 (make-vector 5)))
    (begin 
      (vector-set! v1 0 0)
      (vector-set! v1 1 1)
      (vector-set! v1 2 2)
      (vector-set! v1 3 3)
      (vector-set! v2 0 10)
      (vector-set! v2 1 20)
      (vector-set! v2 2 30)
      (vector-set! v2 3 40)
      (+ (vector-ref v1 3) (vector-ref v2 2)))))

(define (test-let)
  (let ((a 1) (b 5))
    (+ a b)))

(define (test-vector-init)
    (test-vector (make-vector 10) (cons 99 9) (make-vector 5)))

(define (test-vector v1 co v2)
  (begin
    (print-int (car co))
    (vector-set! v1 6 1)
    (vector-set! v1 7 2)
    (vector-set! v1 8 3)
    (vector-set! v1 9 4)
    (vector-set! v2 0 10)
    (vector-set! v2 1 20)
    (vector-set! v2 2 30)
    (vector-set! v2 3 40)
    (print-int (cdr co))
    (print-int (vector-ref v1 6))
    (print-int (vector-ref v1 7))
    (print-int (vector-ref v1 8))
    (print-int (vector-ref v1 9))
    (print-int (vector-ref v2 0))
    (print-int (vector-ref v2 1))
    (print-int (vector-ref v2 2))
    (print-int (vector-ref v2 3))))

(define (test-cons-init)
  (test-cons (cons 1 2) (cons 3 4) (cons 5 6) (cons 7 8)))

(define (test-cons a b c d)
  (begin
  (print-int (car a))
  (print-int (cdr a))
  (print-int (car b))
  (print-int (cdr b))
  (print-int (car c))
  (print-int (cdr c))
  (print-int (car d))
  (print-int (cdr d))))

(define (test-cons-2-init)
  (begin 
    (print-int (car (cons 1 2)))
    (print-int (car (cons 10 20)))
    (print-int (car (cons 11 21)))
    (print-int (car (cons 12 22)))
    (print-int (car (cons 13 23)))
    (print-int (car (cons 5 6)))))


(comment "dtcm section has 16KB")
(comment "One fun frame takes 52 bytes = 13 variables * 4 bytes")
(comment "the function is called 242 times, the whole stack is 12584 bytes")
(define (dtcm-stack-size i)
  (begin (print-int i) (dtcm-stack-size (+ i 1))))


(define (power6 i)
  (if (<= i 1)
    1
    (begin (print-int i) (power (- i 1)))))

(define (power i)
  (if (<= i 1)
    1
    (begin (print-int i) (* i (power (- i 1))))))

(define (poweri i)
  5)

(define (power2 i)
  (cond ((<= i 1) 1)
        (else (* i (power (- i 1))))))

(define (scheme_entry2)
  (cond ((< 2 1) 1)
        ((eq? 3 2) 2)
        (else (print_bool #f))))

(define (print i)
  (cond (#f 10)
        (#t (cond (#f 11)
                  (#t (print_int 32))
                  (#t (print 1))
                  (#t (print 1 2))
                  (#t (print_bool #t))
                  (#t (cond (#f 13)
                            (#t (begin 100 (+ 30 (+ 30 (+ 30 (+ 30 30))))))))))))

(comment "ret 15")
