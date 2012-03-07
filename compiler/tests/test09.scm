(comment "declarations of C functions")
(decl-c-fun print_int void (int))
(decl-c-fun print_bool void (bool))


(define (scheme_entry)
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
