(comment "declarations of C functions")
(c-fun print-int void (int))
(c-fun print-bool void (bool))


(comment "declarations of global functions")
(global-fun scheme-entry)

(assembler (scheme-entry mem_addr mem_size)
  (comment "setting stack beginning to SP")
  (MOV SP, R0)
  (ADD SP, SP, R1)
  (comment "setting heap beginning to SL")
  (MOV SL, R0)
  (comment "running internal function")
  (BL internal_scheme_entry)
  (BX LR))

(assembler (alloc-mem mem_size)
  (comment "allocates specified memory size on the heap")
  (comment "8-byte borders")
  (AND R2, SL, #0b111)
  (CMP R2, #0)
  (BEQ .alloc_alligned)
  (comment "need to align to nearest boundary")
  (AND SL, SL, #0xFFFFFFF8)
  (ADD SL, SL, #0b1000)
  (.alloc_alligned:)
  (comment "heap pointer is aligned")
  (MOV R3, SL)
  (comment "move heap pointer")
  (ADD SL, SL, R0)
  (comment "return")
  (MOV R0, R3)
  (BX LR))

(comment "vector")



(comment "cons")

(assembler (cons c1 c2)
  (comment "constructs cons")
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")
  (comment "allocate memory, 8 bytes")
  (MOV R4, R0)
  (MOV R5, R1)
  (MOV R0, #8)
  (BL alloc_mem)
  (MOV R6, R0)
  (comment "set car")
  (STR R4, [R6])
  (comment "set cdr")
  (ADD R6, R6, #4)
  (STR R5, [R6])
  (comment "tag pair")
  (comment "return")
  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))

(assembler (car c)
  (comment "returns car of cons")
  (LDR R0, [R0])
  (BX LR))

(assembler (cdr c)
  (comment "returns cdr of cons")
  (ADD R0, R0, #4)
  (LDR R0, [R0])
  (BX LR))

(define (internal-scheme-entry)
  (begin
    (print-int (alloc-mem 3))
    (print-int (alloc-mem 3))
    (internal-scheme-entry2)
    (internal-scheme-entry3)))

(define (internal-scheme-entry2)
  (test-cons (cons 1 2) (cons 3 4) (cons 5 6) (cons 7 8)))

(define (test-cons a b c d)
  (begin
  (print-int (car a))
  (print-int (cdr a))
  (print-int (car b))
  (print-int (cdr b))
  (print-int (car c))
  (print-int (cdr c))
  (print-int (car d))))

(define (internal-scheme-entry3)
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




(comment "ASSEMBLER")

(comment "EQUALITY")

(assembler (eq? a b)
  (comment "equality, compares addresses")
  (comment "implemented incorrectly")
  (CMP R0, R1)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(assembler (vector? a)
  (comment "checks if x is a vector")
  (comment "vector has a mask 111")
  (comment "vector has a tag 001")
  (AND R0, R0, #0b111)
  (CMP R0, #0b101)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(assembler (pair? a)
  (comment "checks if x is a pair")
  (comment "pair has a mask 111")
  (comment "pair has a tag 001")
  (AND R0, R0, #0b111)
  (CMP R0, #0b001)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))



(comment "Atom types predicates")

(assembler (atom? a)
  (comment "checks if x is an atom")
  (comment "number has mask 00001")
  (comment "number has tag 00000")
  (AND R0, R0, #1)
  (CMP R0, #0)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


(assembler (number? x)
  (comment "checks if x is a number")
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
