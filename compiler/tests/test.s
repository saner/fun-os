  @ global functions
  .global scheme_entry
  @ declarations of C functions
  @ declarations of global functions
scheme_entry:
  @ def:  scheme-entry mem_addr mem_size
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
   MOV R2, SP
  @ setting stack beginning to SP
   MOV SP, R0
   ADD SP, SP, R1
  @ setting heap beginning to SL
   MOV SL, R2
   STR R0, [SL]
  @ running internal function
   BL internal_scheme_entry
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
alloc_mem:
  @ def:  alloc-mem mem_size
  @ allocates specified memory size on the heap
  @ 8-byte borders
   LDR R1, [SL]
   AND R2, R1, #0b111
   CMP R2, #0
   BEQ .alloc_alligned
  @ need to align to nearest boundary
   AND R1, R1, #0xFFFFFFF8
   ADD R1, R1, #0b1000
   .alloc_alligned:
  @ heap pointer is aligned
   MOV R3, R1
  @ move heap pointer
  @ untag int
   LSR R0, #3
   ADD R1, R1, R0
   STR R1, [SL]
  @ return
   MOV R0, R3
   BX LR
  
  @ vector
make_vector:
  @ def:  make-vector len
  @ constructs a vector
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ allocate memory, (4 + 4 * len) bytes
   MOV R4, R0
  @ untag int
   LSR R0, #3
   ADD R0, R0, #1
   MOV R2, #4
   MOV R3, R0
   MUL R0, R3, R2
  @ tag int
   LSL R0, #3
   ORR R0, R0, #2
   BL alloc_mem
  @ set length
   STR R4, [R0]
  @ tag
   ADD R0, R0, #0b101
  @ return
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
vector_ref:
  @ def:  vector-ref v k
  @ returns element of a vector
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ untag v
   AND R3, R0, #0xFFFFFFF8
  @ untag int k
   LSR R1, #3
  @ get
   MOV R4, #4
   ADD R5, R1, #1
   MUL R6, R4, R5
   ADD R3, R3, R6
   LDR R0, [R3]
  @ return
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
vector_setEM:
  @ def:  vector-set! v k obj
  @ sets element of a vector
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ untag v
   AND R3, R0, #0xFFFFFFF8
  @ untag int k
   LSR R1, #3
  @ set
   MOV R4, #4
   ADD R5, R1, #1
   MUL R6, R4, R5
   ADD R3, R3, R6
   STR R2, [R3]
  @ return
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
vector_length:
  @ def:  vector-length v
  @ returns length of a vector
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   LDR R0, [R0]
   BX LR
  
vectorQM:
  @ def:  vector? v
  @ checks if x is a vector
  @ vector has a mask 111
  @ vector has a tag 001
   AND R0, R0, #0b111
   CMP R0, #0b101
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
  @ cons
cons:
  @ def:  cons c1 c2
  @ constructs cons
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ allocate memory, 8 bytes
   MOV R4, R0
   MOV R5, R1
   MOV R0, #8
  @ tag int
   LSL R0, #3
   ORR R0, R0, #2
   BL alloc_mem
   MOV R6, R0
  @ set car
   STR R4, [R6]
  @ set cdr
   ADD R6, R6, #4
   STR R5, [R6]
  @ tag
   ADD R0, R0, #0b001
  @ return
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
pairQM:
  @ def:  pair? a
  @ checks if x is a pair
  @ pair has a mask 111
  @ pair has a tag 001
   AND R0, R0, #0b111
   CMP R0, #0b001
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
car:
  @ def:  car c
  @ returns car of cons
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   LDR R0, [R0]
   BX LR
  
cdr:
  @ def:  cdr c
  @ returns cdr of cons
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   ADD R0, R0, #4
   LDR R0, [R0]
   BX LR
  
internal_scheme_entry:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-list-exp
  BL test_list_exp
  MOV R1, R0
  @ call end function test-list-exp
  MOV R0, R1
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_list_exp:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ calling function cons
  @ preparing arg _var1
  @ int 1 with name _var1
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var6
  @ calling function cons
  @ preparing arg _var2
  @ int 2 with name _var2
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ preparing arg _var5
  @ calling function cons
  @ preparing arg _var3
  @ int 3 with name _var3
  MOV R2, #3
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ preparing arg _var4
  @ empty list with name _var4
  MOV R3, #0
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R3
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ loading to reg arg 2
  @ loading var "_var2"
  STMFD SP!, {R0}
  LDR R0, [FP, #-8]
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R2
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ loading to reg arg 2
  @ loading var "_var1"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-4]
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R2
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  MOV R3, R2
  @ let block var def end
  @ calling function print_int_c_wrapper
  @ preparing arg _var10
  @ calling function cdr
  @ preparing arg _var9
  @ calling function cdr
  @ preparing arg _var8
  @ calling function cdr
  @ preparing arg ls
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL cdr
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cdr
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL cdr
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cdr
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL cdr
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cdr
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL print_int_c_wrapper
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function print_int_c_wrapper
  @ let block end
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_cons_complex:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ calling function cons
  @ preparing arg _var12
  @ int 1 with name _var12
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var15
  @ calling function cons
  @ preparing arg _var13
  @ int 2 with name _var13
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ preparing arg _var14
  @ int 3 with name _var14
  MOV R2, #3
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R1
  @ loading to reg arg 1
  MOV R1, R2
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ loading to reg arg 2
  @ loading var "_var12"
  STMFD SP!, {R0}
  LDR R0, [FP, #-4]
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R2
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  MOV R3, R2
  @ let block var def end
  @ calling function print_int_c_wrapper
  @ preparing arg _var18
  @ calling function cdr
  @ preparing arg _var17
  @ calling function cdr
  @ preparing arg ls
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL cdr
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cdr
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL cdr
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cdr
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL print_int_c_wrapper
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function print_int_c_wrapper
  @ let block end
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_empty_list:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ empty list with name _var20
  MOV R0, #0
  MOV R0, R0
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_let_2:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var21
  @ int 4 with name _var21
  MOV R0, #4
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL make_vector
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function make-vector
  MOV R2, R1
  @ let block var def end
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var23
  @ int 5 with name _var23
  MOV R3, #5
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL make_vector
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function make-vector
  MOV R4, R3
  @ let block var def end
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var25
  @ int 0 with name _var25
  MOV R5, #0
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var26
  @ int 0 with name _var26
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var28
  @ int 1 with name _var28
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var29
  @ int 1 with name _var29
  MOV R6, #1
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var31
  @ int 2 with name _var31
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var32
  @ int 2 with name _var32
  MOV R6, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var34
  @ int 3 with name _var34
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var35
  @ int 3 with name _var35
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-8]
  @ preparing arg _var37
  @ int 0 with name _var37
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ preparing arg _var38
  @ int 10 with name _var38
  MOV R7, #10
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R5
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R6
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R7
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var40
  @ int 1 with name _var40
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var41
  @ int 20 with name _var41
  MOV R6, #20
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var43
  @ int 2 with name _var43
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var44
  @ int 30 with name _var44
  MOV R6, #30
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var46
  @ int 3 with name _var46
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var47
  @ int 40 with name _var47
  MOV R6, #40
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-60]
  @ preparing arg _var49
  @ int 3 with name _var49
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-8]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-8]
  @ preparing arg _var51
  @ int 2 with name _var51
  MOV R6, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-60]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  @ loading var "_var50"
  LDR R6, [FP, #-116]
  @ untagging int, reg R6
  LSR R6, R6, #3
  @ untagging int, reg R4
  LSR R4, R4, #3
  ADD R5, R6, R4
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ let block end
  MOV R0, R5
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_let:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ int 1 with name _var54
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  MOV R1, R0
  @ let block var def end
  @ let block var def start
  @ int 5 with name _var55
  MOV R2, #5
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  MOV R3, R2
  @ let block var def end
  @ untagging int, reg R1
  LSR R1, R1, #3
  @ untagging int, reg R3
  LSR R3, R3, #3
  ADD R4, R1, R3
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ let block end
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_vector_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-vector
  @ preparing arg _var58
  @ calling function make-vector
  @ preparing arg _var57
  @ int 10 with name _var57
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL make_vector
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function make-vector
  @ preparing arg _var61
  @ calling function cons
  @ preparing arg _var59
  @ int 99 with name _var59
  MOV R2, #99
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ preparing arg _var60
  @ int 9 with name _var60
  MOV R3, #9
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R3
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ preparing arg _var63
  @ calling function make-vector
  @ preparing arg _var62
  @ int 5 with name _var62
  MOV R3, #5
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL make_vector
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function make-vector
  @ loading to reg arg 3
  @ loading var "_var58"
  STMFD SP!, {R0}
  LDR R0, [FP, #-8]
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R2
  @ loading to reg arg 1
  MOV R2, R3
  STMFD SP!, {R0, R1, R2}
  BL test_vector
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function test-vector
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_vector:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var65
  @ calling function car
  @ preparing arg co
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R1
  STMFD SP!, {R0, R2}
  BL car
  MOV R1, R0
  LDMFD SP!, {R0, R2}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R1
  STMFD SP!, {R0, R2}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0, R2}
  @ call end function print_int_c_wrapper
  @ calling function vector-set!
  @ preparing arg v1
  @ loading var "v1"
  LDR R3, [FP, #-4]
  @ preparing arg _var67
  @ int 6 with name _var67
  MOV R4, #6
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var68
  @ int 1 with name _var68
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 2
  @ loading var "_var67"
  STMFD SP!, {R1}
  LDR R1, [FP, #-12]
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R5
  STMFD SP!, {R0, R1, R2}
  BL vector_setEM
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var70
  @ int 7 with name _var70
  MOV R4, #7
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var71
  @ int 2 with name _var71
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  @ loading var "_var70"
  STR R1, [FP, #-12]
  LDR R1, [FP, #-28]
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var73
  @ int 8 with name _var73
  MOV R5, #8
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var74
  @ int 3 with name _var74
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STR R1, [FP, #-28]
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v1
  @ preparing arg _var76
  @ int 9 with name _var76
  MOV R5, #9
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var77
  @ int 4 with name _var77
  MOV R6, #4
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var79
  @ int 0 with name _var79
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ preparing arg _var80
  @ int 10 with name _var80
  MOV R7, #10
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STR R0, [FP, #-4]
  MOV R0, R5
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R6
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R7
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var82
  @ int 1 with name _var82
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var83
  @ int 20 with name _var83
  MOV R6, #20
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var85
  @ int 2 with name _var85
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var86
  @ int 30 with name _var86
  MOV R6, #30
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg v2
  @ preparing arg _var88
  @ int 3 with name _var88
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var89
  @ int 40 with name _var89
  MOV R6, #40
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R5
  @ loading to reg arg 1
  STMFD SP!, {R2}
  MOV R2, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function print_int_c_wrapper
  @ preparing arg _var91
  @ calling function cdr
  @ preparing arg co
  @ loading var "co"
  LDR R5, [FP, #-8]
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STR R0, [FP, #-24]
  MOV R0, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL cdr
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cdr
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var91"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-108]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var94
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var93
  @ int 6 with name _var93
  MOV R6, #6
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-108]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var94"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-120]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var97
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var96
  @ int 7 with name _var96
  MOV R6, #7
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-120]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var97"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-132]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var100
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var99
  @ int 8 with name _var99
  MOV R6, #8
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-132]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var100"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-144]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var103
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var102
  @ int 9 with name _var102
  MOV R6, #9
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-144]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var103"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-156]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var106
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var105
  @ int 0 with name _var105
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-156]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var106"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-168]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var109
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var108
  @ int 1 with name _var108
  MOV R6, #1
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-168]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var109"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-180]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var112
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var111
  @ int 2 with name _var111
  MOV R6, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-180]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var112"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-192]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var115
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var114
  @ int 3 with name _var114
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-192]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var115"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-204]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_cons_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-cons
  @ preparing arg _var119
  @ calling function cons
  @ preparing arg _var117
  @ int 1 with name _var117
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var118
  @ int 2 with name _var118
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ preparing arg _var122
  @ calling function cons
  @ preparing arg _var120
  @ int 3 with name _var120
  MOV R3, #3
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var121
  @ int 4 with name _var121
  MOV R4, #4
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var121"
  STMFD SP!, {R1}
  LDR R1, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL cons
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cons
  @ preparing arg _var125
  @ calling function cons
  @ preparing arg _var123
  @ int 5 with name _var123
  MOV R4, #5
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var124
  @ int 6 with name _var124
  MOV R5, #6
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var123"
  STMFD SP!, {R0}
  LDR R0, [FP, #-16]
  @ loading to reg arg 1
  STR R1, [FP, #-4]
  MOV R1, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  @ preparing arg _var128
  @ calling function cons
  @ preparing arg _var126
  @ int 7 with name _var126
  MOV R5, #7
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var127
  @ int 8 with name _var127
  MOV R6, #8
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-16]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  STMFD SP!, {R4}
  @ loading to reg arg 4
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 3
  STMFD SP!, {R1}
  MOV R1, R3
  @ loading to reg arg 2
  @ loading var "_var125"
  LDR R2, [FP, #-24]
  @ loading to reg arg 1
  @ loading var "_var128"
  LDR R3, [FP, #-32]
  STMFD SP!, {R0, R1, R2, R3}
  BL test_cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function test-cons
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_cons:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var130
  @ calling function car
  @ preparing arg a
  @ loading to reg arg 1
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var130"
  STMFD SP!, {R0}
  LDR R0, [FP, #-4]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var132
  @ calling function cdr
  @ preparing arg a
  @ loading var "a"
  LDR R5, [FP, #-8]
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL cdr
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cdr
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var132"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-16]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var134
  @ calling function car
  @ preparing arg b
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STR R0, [FP, #-16]
  MOV R0, R1
  STMFD SP!, {R0, R2, R3}
  BL car
  MOV R1, R0
  LDMFD SP!, {R0, R2, R3}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R1
  STMFD SP!, {R0, R2, R3}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var136
  @ calling function cdr
  @ preparing arg b
  @ loading var "b"
  LDR R4, [FP, #-24]
  STR R4, [FP, #-24]
  @ loading to reg arg 1
  @ loading var "b"
  STMFD SP!, {R0}
  LDR R0, [FP, #-24]
  STMFD SP!, {R0, R1, R2, R3}
  BL cdr
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cdr
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var136"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-32]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var138
  @ calling function car
  @ preparing arg c
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STR R0, [FP, #-32]
  MOV R0, R2
  STMFD SP!, {R0, R1, R3}
  BL car
  MOV R2, R0
  LDMFD SP!, {R0, R1, R3}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R2
  STMFD SP!, {R0, R1, R3}
  BL print_int_c_wrapper
  MOV R2, R0
  LDMFD SP!, {R0, R1, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var140
  @ calling function cdr
  @ preparing arg c
  @ loading var "c"
  LDR R4, [FP, #-40]
  STR R4, [FP, #-40]
  @ loading to reg arg 1
  @ loading var "c"
  STMFD SP!, {R0}
  LDR R0, [FP, #-40]
  STMFD SP!, {R0, R1, R2, R3}
  BL cdr
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cdr
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var140"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-48]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var142
  @ calling function car
  @ preparing arg d
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STR R0, [FP, #-48]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL car
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL print_int_c_wrapper
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var144
  @ calling function cdr
  @ preparing arg d
  @ loading var "d"
  LDR R4, [FP, #-56]
  STR R4, [FP, #-56]
  @ loading to reg arg 1
  @ loading var "d"
  STMFD SP!, {R0}
  LDR R0, [FP, #-56]
  STMFD SP!, {R0, R1, R2, R3}
  BL cdr
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cdr
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var144"
  STR R0, [FP, #-56]
  LDR R0, [FP, #-64]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
test_cons_2_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var149
  @ calling function car
  @ preparing arg _var148
  @ calling function cons
  @ preparing arg _var146
  @ int 1 with name _var146
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var147
  @ int 2 with name _var147
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL car
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL print_int_c_wrapper
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var154
  @ calling function car
  @ preparing arg _var153
  @ calling function cons
  @ preparing arg _var151
  @ int 10 with name _var151
  MOV R3, #10
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var152
  @ int 20 with name _var152
  MOV R4, #20
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var152"
  STMFD SP!, {R1}
  LDR R1, [FP, #-12]
  STMFD SP!, {R0, R1, R2}
  BL cons
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cons
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL car
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function car
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL print_int_c_wrapper
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var159
  @ calling function car
  @ preparing arg _var158
  @ calling function cons
  @ preparing arg _var156
  @ int 11 with name _var156
  MOV R4, #11
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var157
  @ int 21 with name _var157
  MOV R5, #21
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var156"
  STMFD SP!, {R0}
  LDR R0, [FP, #-32]
  @ loading to reg arg 1
  STR R1, [FP, #-12]
  MOV R1, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var158"
  STR R0, [FP, #-32]
  LDR R0, [FP, #-40]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var159"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-44]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var164
  @ calling function car
  @ preparing arg _var163
  @ calling function cons
  @ preparing arg _var161
  @ int 12 with name _var161
  MOV R5, #12
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var162
  @ int 22 with name _var162
  MOV R6, #22
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-44]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var163"
  STMFD SP!, {R0}
  LDR R0, [FP, #-56]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var164"
  STR R0, [FP, #-56]
  LDR R0, [FP, #-64]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var169
  @ calling function car
  @ preparing arg _var168
  @ calling function cons
  @ preparing arg _var166
  @ int 13 with name _var166
  MOV R5, #13
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var167
  @ int 23 with name _var167
  MOV R6, #23
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-64]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var168"
  STMFD SP!, {R0}
  LDR R0, [FP, #-76]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var169"
  STR R0, [FP, #-76]
  LDR R0, [FP, #-84]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var174
  @ calling function car
  @ preparing arg _var173
  @ calling function cons
  @ preparing arg _var171
  @ int 5 with name _var171
  MOV R5, #5
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var172
  @ int 6 with name _var172
  MOV R6, #6
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-84]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R6
  STMFD SP!, {R0, R1, R2, R3}
  BL cons
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function cons
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var173"
  STMFD SP!, {R0}
  LDR R0, [FP, #-96]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var174"
  STR R0, [FP, #-96]
  LDR R0, [FP, #-104]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  @ dtcm section has 16KB
  @ One fun frame takes 52 bytes = 13 variables * 4 bytes
  @ the function is called 242 times, the whole stack is 12584 bytes
dtcm_stack_size:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg i
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ calling function dtcm-stack-size
  @ preparing arg _var178
  @ int 1 with name _var177
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  ADD R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL dtcm_stack_size
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function dtcm-stack-size
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
power6:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @  -- comp LE -- 
  @ int 1 with name _var180
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  CMP R0, R1
  BLE .Ltrue_0
.Lfalse_1:
  MOV R2, #4
  B .Lend_2
.Ltrue_0:
  MOV R2, #12
.Lend_2:
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  CMP R2, #12
  BNE .Lelse_4
.Lthen_3:
  @ then block
  @ int 1 with name _var183
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var182"
  LDR R3, [FP, #-16]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-16]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-16
  B .Lend_5
.Lelse_4:
  @ else block
  @ calling function print_int_c_wrapper
  @ preparing arg i
  @ loading var "i"
  LDR R0, [FP, #-4]
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ calling function power
  @ preparing arg _var186
  @ int 1 with name _var185
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  SUB R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL power
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function power
  @ loading var "_var182"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var187"
  LDR R4, [FP, #-20]
  MOV R3, R4
  @ else dump regs
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STR R3, [FP, #-16]
  STR R4, [FP, #-20]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-16
.Lend_5:
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
power:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @  -- comp LE -- 
  @ int 1 with name _var188
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  CMP R0, R1
  BLE .Ltrue_6
.Lfalse_7:
  MOV R2, #4
  B .Lend_8
.Ltrue_6:
  MOV R2, #12
.Lend_8:
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  CMP R2, #12
  BNE .Lelse_10
.Lthen_9:
  @ then block
  @ int 1 with name _var191
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var190"
  LDR R3, [FP, #-16]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-16]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-16
  B .Lend_11
.Lelse_10:
  @ else block
  @ calling function print_int_c_wrapper
  @ preparing arg i
  @ loading var "i"
  LDR R0, [FP, #-4]
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ calling function power
  @ preparing arg _var194
  @ int 1 with name _var193
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  SUB R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL power
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function power
  @ loading var "i"
  LDR R5, [FP, #-4]
  @ untagging int, reg R5
  LSR R5, R5, #3
  @ untagging int, reg R3
  LSR R3, R3, #3
  MUL R4, R5, R3
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ loading var "_var190"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  MOV R3, R4
  @ else dump regs
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STR R3, [FP, #-16]
  STMFD SP!, {R4}
  STR R5, [FP, #-4]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-16
.Lend_11:
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
poweri:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ int 5 with name _var197
  MOV R1, #5
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  MOV R0, R1
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
power2:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @  -- comp LE -- 
  @ int 1 with name _var198
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  CMP R0, R1
  BLE .Ltrue_12
.Lfalse_13:
  MOV R2, #4
  B .Lend_14
.Ltrue_12:
  MOV R2, #12
.Lend_14:
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  CMP R2, #12
  BNE .Lelse_16
.Lthen_15:
  @ then block
  @ int 1 with name _var201
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var200"
  LDR R3, [FP, #-16]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-16]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-16
  B .Lend_17
.Lelse_16:
  @ else block
  @ loading var "i"
  LDR R0, [FP, #-4]
  @ calling function power
  @ preparing arg _var203
  @ int 1 with name _var202
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ untagging int, reg R0
  LSR R0, R0, #3
  @ untagging int, reg R1
  LSR R1, R1, #3
  SUB R2, R0, R1
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL power
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function power
  @ loading var "i"
  LDR R4, [FP, #-4]
  @ untagging int, reg R4
  LSR R4, R4, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  MUL R3, R4, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ loading var "_var200"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var205"
  LDR R5, [FP, #-20]
  MOV R3, R5
  @ else dump regs
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STR R3, [FP, #-16]
  STR R4, [FP, #-4]
  STR R5, [FP, #-20]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-16
.Lend_17:
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
scheme_entry2:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @  -- comp LT -- 
  @ int 2 with name _var206
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ int 1 with name _var207
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  CMP R0, R1
  BLT .Ltrue_18
.Lfalse_19:
  MOV R2, #4
  B .Lend_20
.Ltrue_18:
  MOV R2, #12
.Lend_20:
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  CMP R2, #12
  BNE .Lelse_22
.Lthen_21:
  @ then block
  @ int 1 with name _var210
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var209"
  LDR R3, [FP, #-16]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-16]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-16
  B .Lend_23
.Lelse_22:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ calling function eq?
  @ preparing arg _var211
  @ int 3 with name _var211
  MOV R0, #3
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var212
  @ int 2 with name _var212
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL eqQM
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function eq?
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  CMP R2, #12
  BNE .Lelse_25
.Lthen_24:
  @ then block
  @ int 2 with name _var215
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var214"
  LDR R3, [FP, #-32]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-32]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-32
  B .Lend_26
.Lelse_25:
  @ else block
  @ calling function print_bool
  @ preparing arg _var216
  @ bool False with name _var216
  MOV R0, #0
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_bool
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_bool
  @ loading var "_var214"
  LDR R3, [FP, #-32]
  @ else reg move
  MOV R3, R1
  @ else dump regs
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STR R3, [FP, #-32]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-32
.Lend_26:
  @ loading var "_var209"
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var214"
  LDR R0, [FP, #-32]
  MOV R3, R0
  @ else dump regs
  STR R0, [FP, #-32]
  STR R3, [FP, #-16]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-16
.Lend_23:
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
print:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool False with name _var218
  MOV R1, #0
  @ tagging bool, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  CMP R1, #12
  BNE .Lelse_28
.Lthen_27:
  @ then block
  @ int 10 with name _var220
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var219"
  LDR R2, [FP, #-12]
  @ then reg move
  MOV R2, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R2, [FP, #-12]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-12
  B .Lend_29
.Lelse_28:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var221
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_31
.Lthen_30:
  @ then block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool False with name _var223
  MOV R0, #0
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_34
.Lthen_33:
  @ then block
  @ int 11 with name _var225
  MOV R0, #11
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var224"
  LDR R1, [FP, #-28]
  @ then reg move
  MOV R1, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-28]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-28
  B .Lend_35
.Lelse_34:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var226
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_37
.Lthen_36:
  @ then block
  @ calling function print_int
  @ preparing arg _var228
  @ int 32 with name _var228
  MOV R0, #32
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int
  @ loading var "_var227"
  STMFD SP!, {R1}
  LDR R1, [FP, #-36]
  @ then reg move
  @ loading var "_var229"
  LDR R2, [FP, #-40]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-36]
  STR R2, [FP, #-40]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-36
  B .Lend_38
.Lelse_37:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var230
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_40
.Lthen_39:
  @ then block
  @ calling function print
  @ preparing arg _var232
  @ int 1 with name _var232
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print
  @ loading var "_var231"
  STMFD SP!, {R1}
  LDR R1, [FP, #-44]
  @ then reg move
  @ loading var "_var233"
  LDR R2, [FP, #-48]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-44]
  STR R2, [FP, #-48]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-44
  B .Lend_41
.Lelse_40:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var234
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_43
.Lthen_42:
  @ then block
  @ calling function print
  @ preparing arg _var236
  @ int 1 with name _var236
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var237
  @ int 2 with name _var237
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL print
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function print
  @ loading var "_var235"
  STMFD SP!, {R1}
  LDR R1, [FP, #-52]
  @ then reg move
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-52]
  STMFD SP!, {R2}
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-52
  B .Lend_44
.Lelse_43:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var239
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_46
.Lthen_45:
  @ then block
  @ calling function print_bool
  @ preparing arg _var241
  @ bool True with name _var241
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_bool
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_bool
  @ loading var "_var240"
  STMFD SP!, {R1}
  LDR R1, [FP, #-60]
  @ then reg move
  @ loading var "_var242"
  LDR R2, [FP, #-64]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-60]
  STR R2, [FP, #-64]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-60
  B .Lend_47
.Lelse_46:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var243
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_49
.Lthen_48:
  @ then block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool False with name _var245
  MOV R0, #0
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_52
.Lthen_51:
  @ then block
  @ int 13 with name _var247
  MOV R0, #13
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var246"
  LDR R1, [FP, #-76]
  @ then reg move
  MOV R1, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-76]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-76
  B .Lend_53
.Lelse_52:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var248
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_55
.Lthen_54:
  @ then block
  @ int 100 with name _var250
  MOV R0, #100
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ int 30 with name _var251
  MOV R1, #30
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ int 30 with name _var252
  MOV R2, #30
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ int 30 with name _var253
  MOV R3, #30
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ int 30 with name _var254
  MOV R4, #30
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ int 30 with name _var255
  MOV R5, #30
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ untagging int, reg R4
  LSR R4, R4, #3
  @ untagging int, reg R5
  LSR R5, R5, #3
  ADD R6, R4, R5
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ untagging int, reg R3
  LSR R3, R3, #3
  @ untagging int, reg R6
  LSR R6, R6, #3
  ADD R7, R3, R6
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ untagging int, reg R2
  LSR R2, R2, #3
  @ untagging int, reg R7
  LSR R7, R7, #3
  ADD R8, R2, R7
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  @ untagging int, reg R1
  LSR R1, R1, #3
  @ untagging int, reg R8
  LSR R8, R8, #3
  ADD R9, R1, R8
  @ tagging int, reg R9
  LSL R9, R9, #3
  ORR R9, R9, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  @ loading var "_var249"
  STMFD SP!, {R1}
  LDR R1, [FP, #-84]
  @ then reg move
  MOV R1, R9
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-84]
  STMFD SP!, {R2}
  STMFD SP!, {R3}
  STMFD SP!, {R4}
  STMFD SP!, {R5}
  STMFD SP!, {R6}
  STMFD SP!, {R7}
  STMFD SP!, {R8}
  STMFD SP!, {R9}
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-84
  B .Lend_56
.Lelse_55:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var249"
  LDR R1, [FP, #-84]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-84]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-84
.Lend_56:
  @ loading var "_var246"
  LDR R1, [FP, #-76]
  @ else reg move
  @ loading var "_var249"
  LDR R0, [FP, #-84]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-84]
  STR R1, [FP, #-76]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-76
.Lend_53:
  @ loading var "_var244"
  LDR R1, [FP, #-68]
  @ then reg move
  @ loading var "_var246"
  LDR R0, [FP, #-76]
  MOV R1, R0
  @ then dump regs
  STR R0, [FP, #-76]
  STR R1, [FP, #-68]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-68
  B .Lend_50
.Lelse_49:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var244"
  LDR R1, [FP, #-68]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-68]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-68
.Lend_50:
  @ loading var "_var240"
  LDR R1, [FP, #-60]
  @ else reg move
  @ loading var "_var244"
  LDR R0, [FP, #-68]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-68]
  STR R1, [FP, #-60]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-60
.Lend_47:
  @ loading var "_var235"
  LDR R1, [FP, #-52]
  @ else reg move
  @ loading var "_var240"
  LDR R0, [FP, #-60]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-60]
  STR R1, [FP, #-52]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-52
.Lend_44:
  @ loading var "_var231"
  LDR R1, [FP, #-44]
  @ else reg move
  @ loading var "_var235"
  LDR R0, [FP, #-52]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-52]
  STR R1, [FP, #-44]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-44
.Lend_41:
  @ loading var "_var227"
  LDR R1, [FP, #-36]
  @ else reg move
  @ loading var "_var231"
  LDR R0, [FP, #-44]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-44]
  STR R1, [FP, #-36]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-36
.Lend_38:
  @ loading var "_var224"
  LDR R1, [FP, #-28]
  @ else reg move
  @ loading var "_var227"
  LDR R0, [FP, #-36]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-36]
  STR R1, [FP, #-28]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-28
.Lend_35:
  @ loading var "_var222"
  LDR R1, [FP, #-20]
  @ then reg move
  @ loading var "_var224"
  LDR R0, [FP, #-28]
  MOV R1, R0
  @ then dump regs
  STR R0, [FP, #-28]
  STR R1, [FP, #-20]
  @ then reset SP
  MOV SP, FP
  SUB SP, SP, #-20
  B .Lend_32
.Lelse_31:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var222"
  LDR R1, [FP, #-20]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-20]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-20
.Lend_32:
  @ loading var "_var219"
  LDR R2, [FP, #-12]
  @ else reg move
  @ loading var "_var222"
  LDR R0, [FP, #-20]
  MOV R2, R0
  @ else dump regs
  STR R0, [FP, #-20]
  STR R2, [FP, #-12]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-12
.Lend_29:
  MOV R0, R2
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  @ ret 15
  @ ASSEMBLER
  @ EQUALITY
eqQM:
  @ def:  eq? a b
  @ equality, compares addresses
  @ implemented incorrectly
   CMP R0, R1
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
  @ Atom types predicates
atomQM:
  @ def:  atom? a
  @ checks if x is an atom
  @ number has mask 00001
  @ number has tag 00000
   AND R0, R0, #1
   CMP R0, #0
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
numberQM:
  @ def:  number? x
  @ checks if x is a number
  @ number has mask 00111
  @ number has tag 00010
   AND R0, R0, #7
   CMP R0, #2
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
booleanQM:
  @ def:  boolean? x
  @ checks if x is boolean
  @ number has mask 00111
  @ number has tag 00100
   AND R0, R0, #7
   CMP R0, #4
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
  @ Reference types predicates
referenceQM:
  @ def:  reference? a
  @ checks if x is reference
  @ number has mask 00001
  @ number has tag 00001
   AND R0, R0, #1
   CMP R0, #1
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
  @ C functions wrappers
print_int_c_wrapper:
  @ def:  print_int_c_wrapper
  @ body
   STMFD SP!, {LR}
  @ untagging int, reg R0
   LSR R0, R0, #3
   BL print_int
   LDMFD SP!, {LR}
   BX LR
  
print_bool_c_wrapper:
  @ def:  print_bool_c_wrapper
  @ body
   STMFD SP!, {LR}
  @ untagging bool, reg R0
   LSR R0, R0, #3
   BL print_bool
   LDMFD SP!, {LR}
   BX LR
  

