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
  @ pair has