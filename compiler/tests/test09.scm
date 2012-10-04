(comment "declarations of C functions")
(c-fun print-int void (int))
(c-fun print-bool void (bool))


(comment "declarations of global functions")
(global-fun scheme-entry)

(define (scheme-entry)
  (dtcm-stack-size 0))



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




(comment "ASSEMBLER")

(comment "EQUALITY")

(assembler (eq? a b)
  (comment "equality, compares addresses")
  (comment "implemented incorrectly")
  (CMP R0, R1)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(comment "Atom types predicates")

(assembler (atom? a)
  (comment "checks if x is atom")
  (comment "number has mask 00001")
  (comment "number has tag 00000")
  (AND R0, R0, #1)
  (CMP R0, #0)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(assembler (number? x)
  (comment "checks if x is number")
  (comment "number has mask 00011")
  (comment "number has tag 00010")
  (AND R0, R0, #3)
  (CMP R0, #2)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))

(assembler (boolean? x)
  (comment "checks if x is boolean")
  (comment "number has mask 00111")
  (comment "number has tag 00100")
  (AND R0, R0, #7)
  (CMP R0, #4)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(comment "Reference types predicates")

(assembler (reference? a)
  (comment "checks if x is reference")
  (comment "number has mask 00001")
  (comment "number has tag 00001")
  (AND R0, R0, #1)
  (CMP R0, #1)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))
