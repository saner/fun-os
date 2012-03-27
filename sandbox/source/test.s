  @ global functions
  .global scheme_entry
  @ declarations of C functions
  @ declarations of global functions
scheme_entry:
  @ def:  scheme-entry mem_addr mem_size
  @ setting stack beginning to SP
   MOV SP, R0
   ADD SP, SP, R1
  @ setting heap beginning to SL
   MOV SL, R0
  @ running internal function
   BL internal_scheme_entry
   BX LR
  
alloc_mem:
  @ def:  alloc-mem mem_size
  @ allocates specified memory size on the heap
  @ 8-byte borders
   AND R2, SL, #0b111
   CMP R2, #0
   BEQ .alloc_alligned
  @ need to align to nearest boundary
   AND SL, SL, #0xFFFFFFF8
   ADD SL, SL, #0b1000
   .alloc_alligned:
  @ heap pointer is aligned
   MOV R3, SL
  @ move heap pointer
   ADD SL, SL, R0
  @ return
   MOV R0, R3
   BX LR
  
  @ vector
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
   BL alloc_mem
   MOV R6, R0
  @ set car
   STR R4, [R6]
  @ set cdr
   ADD R6, R6, #4
   STR R5, [R6]
  @ tag pair
  @ return
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
car:
  @ def:  car c
  @ returns car of cons
   LDR R0, [R0]
   BX LR
  
cdr:
  @ def:  cdr c
  @ returns cdr of cons
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
  @ calling function print_int_c_wrapper
  @ preparing arg _var1
  @ calling function alloc-mem
  @ preparing arg _var0
  @ int 3 with name _var0
  MOV R0, #3
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL alloc_mem
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function alloc-mem
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R1
  STMFD SP!, {R0}
  BL print_int
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ int 9 with name _var0
  MOV R0, #9
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL alloc_mem
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function alloc-mem
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R1
  STMFD SP!, {R0}
  BL print_int
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var4
  @ calling function alloc-mem
  @ preparing arg _var3
  @ int 3 with name _var3
  MOV R2, #3
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL alloc_mem
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function alloc-mem
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL print_int
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function print_int_c_wrapper
  @ calling function internal-scheme-entry2
  STMFD SP!, {R0, R1, R2}
  BL internal_scheme_entry2
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function internal-scheme-entry2
  @ calling function internal-scheme-entry3
  STMFD SP!, {R0, R1, R2, R3}
  BL internal_scheme_entry3
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function internal-scheme-entry3
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
internal_scheme_entry2:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-cons
  @ preparing arg _var10
  @ calling function cons
  @ preparing arg _var8
  @ int 1 with name _var8
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var9
  @ int 2 with name _var9
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ preparing arg _var13
  @ calling function cons
  @ preparing arg _var11
  @ int 3 with name _var11
  MOV R3, #3
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ preparing arg _var12
  @ int 4 with name _var12
  MOV R4, #4
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var12"
  STMFD SP!, {R1}
  LDR R1, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL cons
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cons
  @ preparing arg _var16
  @ calling function cons
  @ preparing arg _var14
  @ int 5 with name _var14
  MOV R4, #5
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ preparing arg _var15
  @ int 6 with name _var15
  MOV R5, #6
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var14"
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
  @ preparing arg _var19
  @ calling function cons
  @ preparing arg _var17
  @ int 7 with name _var17
  MOV R5, #7
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ preparing arg _var18
  @ int 8 with name _var18
  MOV R6, #8
  @ tagging int, reg R6
  LSL R6, R6, #2
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
  @ loading var "_var16"
  LDR R2, [FP, #-24]
  @ loading to reg arg 1
  @ loading var "_var19"
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
  @ preparing arg _var21
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
  @ loading var "_var21"
  STMFD SP!, {R0}
  LDR R0, [FP, #-4]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var23
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
  @ loading var "_var23"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-16]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var25
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
  @ preparing arg _var27
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
  @ loading var "_var27"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-32]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var29
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
  @ preparing arg _var31
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
  @ loading var "_var31"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-48]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var33
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
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
internal_scheme_entry3:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var38
  @ calling function car
  @ preparing arg _var37
  @ calling function cons
  @ preparing arg _var35
  @ int 1 with name _var35
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var36
  @ int 2 with name _var36
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ preparing arg _var43
  @ calling function car
  @ preparing arg _var42
  @ calling function cons
  @ preparing arg _var40
  @ int 10 with name _var40
  MOV R3, #10
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ preparing arg _var41
  @ int 20 with name _var41
  MOV R4, #20
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var41"
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
  @ preparing arg _var48
  @ calling function car
  @ preparing arg _var47
  @ calling function cons
  @ preparing arg _var45
  @ int 11 with name _var45
  MOV R4, #11
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ preparing arg _var46
  @ int 21 with name _var46
  MOV R5, #21
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var45"
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
  @ loading var "_var47"
  STR R0, [FP, #-32]
  LDR R0, [FP, #-40]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var48"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-44]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var53
  @ calling function car
  @ preparing arg _var52
  @ calling function cons
  @ preparing arg _var50
  @ int 12 with name _var50
  MOV R5, #12
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ preparing arg _var51
  @ int 22 with name _var51
  MOV R6, #22
  @ tagging int, reg R6
  LSL R6, R6, #2
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
  @ loading var "_var52"
  STMFD SP!, {R0}
  LDR R0, [FP, #-56]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var53"
  STR R0, [FP, #-56]
  LDR R0, [FP, #-64]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var58
  @ calling function car
  @ preparing arg _var57
  @ calling function cons
  @ preparing arg _var55
  @ int 13 with name _var55
  MOV R5, #13
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ preparing arg _var56
  @ int 23 with name _var56
  MOV R6, #23
  @ tagging int, reg R6
  LSL R6, R6, #2
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
  @ loading var "_var57"
  STMFD SP!, {R0}
  LDR R0, [FP, #-76]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var58"
  STR R0, [FP, #-76]
  LDR R0, [FP, #-84]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var63
  @ calling function car
  @ preparing arg _var62
  @ calling function cons
  @ preparing arg _var60
  @ int 5 with name _var60
  MOV R5, #5
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ preparing arg _var61
  @ int 6 with name _var61
  MOV R6, #6
  @ tagging int, reg R6
  LSL R6, R6, #2
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
  @ loading var "_var62"
  STMFD SP!, {R0}
  LDR R0, [FP, #-96]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var63"
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
  @ preparing arg _var67
  @ int 1 with name _var66
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #2
  @ untagging int, reg R2
  LSR R2, R2, #2
  ADD R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #2
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
  @ int 1 with name _var69
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 1 with name _var72
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var71"
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
  @ preparing arg _var75
  @ int 1 with name _var74
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #2
  @ untagging int, reg R2
  LSR R2, R2, #2
  SUB R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ loading to reg arg 1
  STR R0, [FP, #-4]
  MOV R0, R3
  STMFD SP!, {R0, R1, R2}
  BL power
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function power
  @ loading var "_var71"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var76"
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
  @ int 1 with name _var77
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 1 with name _var80
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var79"
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
  @ preparing arg _var83
  @ int 1 with name _var82
  MOV R2, #1
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ untagging int, reg R0
  LSR R0, R0, #2
  @ untagging int, reg R2
  LSR R2, R2, #2
  SUB R3, R0, R2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ tagging int, reg R2
  LSL R2, R2, #2
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
  LSR R5, R5, #2
  @ untagging int, reg R3
  LSR R3, R3, #2
  MUL R4, R5, R3
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ loading var "_var79"
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
  @ int 5 with name _var86
  MOV R1, #5
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 1 with name _var87
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 1 with name _var90
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var89"
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
  @ preparing arg _var92
  @ int 1 with name _var91
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ untagging int, reg R0
  LSR R0, R0, #2
  @ untagging int, reg R1
  LSR R1, R1, #2
  SUB R2, R0, R1
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  LSR R4, R4, #2
  @ untagging int, reg R2
  LSR R2, R2, #2
  MUL R3, R4, R2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ loading var "_var89"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var94"
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
  @ int 2 with name _var95
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 1 with name _var96
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 1 with name _var99
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var98"
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
  @ preparing arg _var100
  @ int 3 with name _var100
  MOV R0, #3
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var101
  @ int 2 with name _var101
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
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
  @ int 2 with name _var104
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var103"
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
  @ preparing arg _var105
  @ bool False with name _var105
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
  @ loading var "_var103"
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
  @ loading var "_var98"
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var103"
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
  @ bool False with name _var107
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
  @ int 10 with name _var109
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var108"
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
  @ bool True with name _var110
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
  @ bool False with name _var112
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
  @ int 11 with name _var114
  MOV R0, #11
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var113"
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
  @ bool True with name _var115
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
  @ preparing arg _var117
  @ int 32 with name _var117
  MOV R0, #32
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int
  @ loading var "_var116"
  STMFD SP!, {R1}
  LDR R1, [FP, #-36]
  @ then reg move
  @ loading var "_var118"
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
  @ bool True with name _var119
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
  @ preparing arg _var121
  @ int 1 with name _var121
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print
  @ loading var "_var120"
  STMFD SP!, {R1}
  LDR R1, [FP, #-44]
  @ then reg move
  @ loading var "_var122"
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
  @ bool True with name _var123
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
  @ preparing arg _var125
  @ int 1 with name _var125
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var126
  @ int 2 with name _var126
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R0, R1}
  BL print
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function print
  @ loading var "_var124"
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
  @ bool True with name _var128
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
  @ preparing arg _var130
  @ bool True with name _var130
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
  @ loading var "_var129"
  STMFD SP!, {R1}
  LDR R1, [FP, #-60]
  @ then reg move
  @ loading var "_var131"
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
  @ bool True with name _var132
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
  @ bool False with name _var134
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
  @ int 13 with name _var136
  MOV R0, #13
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var135"
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
  @ bool True with name _var137
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
  @ int 100 with name _var139
  MOV R0, #100
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 30 with name _var140
  MOV R1, #30
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ int 30 with name _var141
  MOV R2, #30
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ int 30 with name _var142
  MOV R3, #30
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ int 30 with name _var143
  MOV R4, #30
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ int 30 with name _var144
  MOV R5, #30
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ untagging int, reg R4
  LSR R4, R4, #2
  @ untagging int, reg R5
  LSR R5, R5, #2
  ADD R6, R4, R5
  @ tagging int, reg R6
  LSL R6, R6, #2
  ORR R6, R6, #2
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ tagging int, reg R5
  LSL R5, R5, #2
  ORR R5, R5, #2
  @ untagging int, reg R3
  LSR R3, R3, #2
  @ untagging int, reg R6
  LSR R6, R6, #2
  ADD R7, R3, R6
  @ tagging int, reg R7
  LSL R7, R7, #2
  ORR R7, R7, #2
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ tagging int, reg R6
  LSL R6, R6, #2
  ORR R6, R6, #2
  @ untagging int, reg R2
  LSR R2, R2, #2
  @ untagging int, reg R7
  LSR R7, R7, #2
  ADD R8, R2, R7
  @ tagging int, reg R8
  LSL R8, R8, #2
  ORR R8, R8, #2
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ tagging int, reg R7
  LSL R7, R7, #2
  ORR R7, R7, #2
  @ untagging int, reg R1
  LSR R1, R1, #2
  @ untagging int, reg R8
  LSR R8, R8, #2
  ADD R9, R1, R8
  @ tagging int, reg R9
  LSL R9, R9, #2
  ORR R9, R9, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ tagging int, reg R8
  LSL R8, R8, #2
  ORR R8, R8, #2
  @ loading var "_var138"
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
  @ loading var "_var138"
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
  @ loading var "_var135"
  LDR R1, [FP, #-76]
  @ else reg move
  @ loading var "_var138"
  LDR R0, [FP, #-84]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-84]
  STR R1, [FP, #-76]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-76
.Lend_53:
  @ loading var "_var133"
  LDR R1, [FP, #-68]
  @ then reg move
  @ loading var "_var135"
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
  @ loading var "_var133"
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
  @ loading var "_var129"
  LDR R1, [FP, #-60]
  @ else reg move
  @ loading var "_var133"
  LDR R0, [FP, #-68]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-68]
  STR R1, [FP, #-60]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-60
.Lend_47:
  @ loading var "_var124"
  LDR R1, [FP, #-52]
  @ else reg move
  @ loading var "_var129"
  LDR R0, [FP, #-60]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-60]
  STR R1, [FP, #-52]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-52
.Lend_44:
  @ loading var "_var120"
  LDR R1, [FP, #-44]
  @ else reg move
  @ loading var "_var124"
  LDR R0, [FP, #-52]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-52]
  STR R1, [FP, #-44]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-44
.Lend_41:
  @ loading var "_var116"
  LDR R1, [FP, #-36]
  @ else reg move
  @ loading var "_var120"
  LDR R0, [FP, #-44]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-44]
  STR R1, [FP, #-36]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-36
.Lend_38:
  @ loading var "_var113"
  LDR R1, [FP, #-28]
  @ else reg move
  @ loading var "_var116"
  LDR R0, [FP, #-36]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-36]
  STR R1, [FP, #-28]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-28
.Lend_35:
  @ loading var "_var111"
  LDR R1, [FP, #-20]
  @ then reg move
  @ loading var "_var113"
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
  @ loading var "_var111"
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
  @ loading var "_var108"
  LDR R2, [FP, #-12]
  @ else reg move
  @ loading var "_var111"
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
  
vectorQM:
  @ def:  vector? a
  @ checks if x is a vector
  @ vector has a mask 111
  @ vector has a tag 001
   AND R0, R0, #0b111
   CMP R0, #0b101
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
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
  @ number has mask 00011
  @ number has tag 00010
   AND R0, R0, #3
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
   LSR R0, R0, #2
   BL print_int
   LDMFD SP!, {LR}
   BX LR
  
print_bool_c_wrapper:
  @ def:  print_bool_c_wrapper
  @ body
   STMFD SP!, {LR}
  @ untagging int, reg R0
   LSR R0, R0, #3
   BL print_bool
   LDMFD SP!, {LR}
   BX LR
  

