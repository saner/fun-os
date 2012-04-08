(comment "global constants")
(comment "interrups start")
(inline "REG_IME: .word 0x04000208")
(inline "REG_IE: .word 0x04000210")
(inline "REG_IF: .word 0x04000214")
(inline "REG_TM0_DAT: .word 0x04000100")
(inline "REG_TM0_CNT: .word 0x04000102")
(inline "INT_HAND_SHIFT: .word 0x3ffc")
(inline "INTERR_HANDLER: .word 0x0b003ffc")
(inline "TM0_EN: .word 0b11000011")
(comment "interrups end")

(comment "declarations of C functions")
(c-fun print-int void (int))
(c-fun print-bool void (bool))

(comment "declarations of global functions")
(global-fun scheme-entry)

(assembler (scheme-entry mem_addr mem_size)
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")

  (MOV R2, SP)
  (comment "setting stack beginning to SP")
  (MOV SP, R0)
  (ADD SP, SP, R1)
  (comment "setting heap beginning to SL")
  (MOV SL, R2)
  (STR R0, [SL])

  (comment "interrupts")
  (BL initialize_interrupts)

  (comment "run code")
  (BL internal_scheme_entry)

  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))

(assembler (initialize-interrupts)
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")

  (comment "interrupt start")
  (comment "disable interrupts, REG_IME = 0")
  (LDR R5, REG_IME)
  (MOV R6, #0)
  (STR R6, [R5])

  (comment "set TM0 value")
  (LDR R5, REG_TM0_DAT)
  (MOV R6, #0)
  (STRH R6, [R5])

  (comment "set TM0 control")
  (comment "enabled, irq, prescale 1024")
  (LDR R5, REG_TM0_CNT)
  (LDR R6, TM0_EN)
  (STRh R6, [R5])

  (comment "enable TM0")
  (LDR R5, REG_IE)
  (LDR R6, [R5])
  (ORR R6, R6, #0b0000)
  (STR R6, [R5])

  (comment "set interrupt handler")
  (LDR R5, INTERR_HANDLER)
  (ADR R6, interrupt_handler)
  (STR R6, [R5])

  (comment "enable interrupts, REG_IME = 1")
  (LDR R5, REG_IME)
  (MOV R6, #0b1)
  (STR R6, [R5])
  (comment "interrupt end")

  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))

(assembler (interrupt-handler)
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")

  (LDR R5, REG_IF)
  (LDR R6, [R5])
  (vblank:)
  (MOV R7, R6)
  (AND R7, R7, #0b0001)
  (CMP R7, #0b0001)
  (BNE timer)
  (MOV R8, #0b0001)
  (timer:)
  (MOV R7, R6)
  (AND R7, R7, #0b1000)
  (CMP R7, #0b1000)
  (BNE end)
  (ORR R8, R8, #0b1000)
  (MOV R0, #55)
  (BL print_int)
  (end:)
  (STR R8, [R5])

  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))
  
(assembler (alloc-mem mem_size)
  (comment "allocates specified memory size on the heap")
  (comment "8-byte borders")
  (LDR R1, [SL])
  (AND R2, R1, #0b111)
  (CMP R2, #0)
  (BEQ .alloc_alligned)
  (comment "need to align to nearest boundary")
  (AND R1, R1, #0xFFFFFFF8)
  (ADD R1, R1, #0b1000)
  (.alloc_alligned:)
  (comment "heap pointer is aligned")
  (MOV R3, R1)
  (comment "move heap pointer")
  (comment "untag int")
  (LSR R0, #3)
  (ADD R1, R1, R0)
  (STR R1, [SL])
  (comment "return")
  (MOV R0, R3)
  (BX LR))

(comment "vector")

(assembler (make-vector len)
  (comment "constructs a vector")
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")
  (comment "allocate memory, (4 + 4 * len) bytes")
  (MOV R4, R0)
  (comment "untag int")
  (LSR R0, #3)
  (ADD R0, R0, #1)
  (MOV R2, #4)
  (MOV R3, R0)
  (MUL R0, R3, R2)
  (comment "tag int")
  (LSL R0, #3)
  (ORR R0, R0, #2)
  (BL alloc_mem)
  (comment "set length")
  (STR R4, [R0])
  (comment "tag")
  (ADD R0, R0, #0b101)
  (comment "return")
  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))

(assembler (vector-ref v k)
  (comment "returns element of a vector")
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")
  (comment "untag v")
  (AND R3, R0, #0xFFFFFFF8)
  (comment "untag int k")
  (LSR R1, #3)
  (comment "get")
  (MOV R4, #4)
  (ADD R5, R1, #1)
  (MUL R6, R4, R5)
  (ADD R3, R3, R6)
  (LDR R0, [R3])
  (comment "return")
  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))


(assembler (vector-set! v k obj)
  (comment "sets element of a vector")
  (comment "prologue start")
  (STMFD SP!, {LR})
  (STMFD SP!, {R4, R5, R6, R7, R8, R9})
  (STMFD SP!, {SL})
  (STMFD SP!, {FP})
  (MOV FP, SP)
  (comment "prologue end")
  (comment "untag v")
  (AND R3, R0, #0xFFFFFFF8)
  (comment "untag int k")
  (LSR R1, #3)
  (comment "set")
  (MOV R4, #4)
  (ADD R5, R1, #1)
  (MUL R6, R4, R5)
  (ADD R3, R3, R6)
  (STR R2, [R3])
  (comment "return")
  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))


(assembler (vector-length v)
  (comment "returns length of a vector")
  (comment "untag")
  (AND R0, R0, #0xFFFFFFF8)
  (comment "return")
  (LDR R0, [R0])
  (BX LR))


(assembler (vector? v)
  (comment "checks if x is a vector")
  (comment "vector has a mask 111")
  (comment "vector has a tag 001")
  (AND R0, R0, #0b111)
  (CMP R0, #0b101)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))


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
  (comment "tag int")
  (LSL R0, #3)
  (ORR R0, R0, #2)
  (BL alloc_mem)
  (MOV R6, R0)
  (comment "set car")
  (STR R4, [R6])
  (comment "set cdr")
  (ADD R6, R6, #4)
  (STR R5, [R6])
  (comment "tag")
  (ADD R0, R0, #0b001)
  (comment "return")
  (comment "epilog start")
  (MOV SP, FP)
  (LDMFD SP!, {FP})
  (LDMFD SP!, {SL})
  (LDMFD SP!, {R4, R5, R6, R7, R8, R9})
  (LDMFD SP!, {LR})
  (BX LR)
  (comment "epilog end"))

(assembler (pair? a)
  (comment "checks if x is a pair")
  (comment "pair has a mask 111")
  (comment "pair has a tag 001")
  (AND R0, R0, #0b111)
  (CMP R0, #0b001)
  (MOVEQ R0, #12)
  (MOVNE R0, #4)
  (BX LR))

(assembler (car c)
  (comment "returns car of cons")
  (comment "untag")
  (AND R0, R0, #0xFFFFFFF8)
  (comment "return")
  (LDR R0, [R0])
  (BX LR))

(assembler (cdr c)
  (comment "returns cdr of cons")
  (comment "untag")
  (AND R0, R0, #0xFFFFFFF8)
  (comment "return")
  (ADD R0, R0, #4)
  (LDR R0, [R0])
  (BX LR))

(define (internal-scheme-entry)
  (test-vector-comp))

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
  (comment "number has mask 00111")
  (comment "number has tag 00010")
  (AND R0, R0, #7)
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