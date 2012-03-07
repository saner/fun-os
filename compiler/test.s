  .global scheme_entry
  @ declarations of C functions
scheme_entry:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @  -- comp LT -- 
  @ int 2 with name _var0
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 1 with name _var1
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  CMP R0, R1
  BLT .Ltrue_0
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
  @ int 1 with name _var4
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var3"
  LDR R3, [FP, #-16]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-16]
  B .Lend_5
.Lelse_4:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ calling function eq?
  @ preparing arg _var5
  @ int 3 with name _var5
  MOV R0, #3
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var6
  @ int 2 with name _var6
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 3
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
  BNE .Lelse_7
.Lthen_6:
  @ then block
  @ int 2 with name _var9
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var8"
  LDR R3, [FP, #-32]
  @ then reg move
  MOV R3, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R3, [FP, #-32]
  B .Lend_8
.Lelse_7:
  @ else block
  @ calling function print_bool_c_wrapper
  @ preparing arg _var10
  @ bool False with name _var10
  MOV R0, #0
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  @ loading to reg arg 3
  STMFD SP!, {R0}
  BL print_bool_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_bool_c_wrapper
  @ loading var "_var8"
  LDR R3, [FP, #-32]
  @ else reg move
  MOV R3, R1
  @ else dump regs
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STR R3, [FP, #-32]
.Lend_8:
  @ loading var "_var3"
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var8"
  LDR R0, [FP, #-32]
  MOV R3, R0
  @ else dump regs
  STR R0, [FP, #-32]
  STR R3, [FP, #-16]
.Lend_5:
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
  @ bool False with name _var12
  MOV R1, #0
  @ tagging bool, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  STMFD SP!, {R2}
  CMP R1, #12
  BNE .Lelse_10
.Lthen_9:
  @ then block
  @ int 10 with name _var14
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var13"
  LDR R2, [FP, #-12]
  @ then reg move
  MOV R2, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R2, [FP, #-12]
  B .Lend_11
.Lelse_10:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var15
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_13
.Lthen_12:
  @ then block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool False with name _var17
  MOV R0, #0
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_16
.Lthen_15:
  @ then block
  @ int 11 with name _var19
  MOV R0, #11
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var18"
  LDR R1, [FP, #-28]
  @ then reg move
  MOV R1, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-28]
  B .Lend_17
.Lelse_16:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var20
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_19
.Lthen_18:
  @ then block
  @ calling function print_int_c_wrapper
  @ preparing arg _var22
  @ int 32 with name _var22
  MOV R0, #32
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading to reg arg 3
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ loading var "_var21"
  STMFD SP!, {R1}
  LDR R1, [FP, #-36]
  @ then reg move
  @ loading var "_var23"
  LDR R2, [FP, #-40]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-36]
  STR R2, [FP, #-40]
  B .Lend_20
.Lelse_19:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var24
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_22
.Lthen_21:
  @ then block
  @ calling function print
  @ preparing arg _var26
  @ int 1 with name _var26
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading to reg arg 3
  STMFD SP!, {R0}
  BL print
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print
  @ loading var "_var25"
  STMFD SP!, {R1}
  LDR R1, [FP, #-44]
  @ then reg move
  @ loading var "_var27"
  LDR R2, [FP, #-48]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-44]
  STR R2, [FP, #-48]
  B .Lend_23
.Lelse_22:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var28
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_25
.Lthen_24:
  @ then block
  @ calling function print
  @ preparing arg _var30
  @ int 1 with name _var30
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var31
  @ int 2 with name _var31
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ loading to reg arg 2
  @ loading to reg arg 3
  STMFD SP!, {R0, R1}
  BL print
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function print
  @ loading var "_var29"
  STMFD SP!, {R1}
  LDR R1, [FP, #-52]
  @ then reg move
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-52]
  STMFD SP!, {R2}
  B .Lend_26
.Lelse_25:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var33
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  STMFD SP!, {R0}
  STMFD SP!, {R1}
  CMP R0, #12
  BNE .Lelse_28
.Lthen_27:
  @ then block
  @ calling function print_bool_c_wrapper
  @ preparing arg _var35
  @ bool True with name _var35
  MOV R0, #1
  @ tagging bool, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #4
  @ loading to reg arg 3
  STMFD SP!, {R0}
  BL print_bool_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_bool_c_wrapper
  @ loading var "_var34"
  STMFD SP!, {R1}
  LDR R1, [FP, #-60]
  @ then reg move
  @ loading var "_var36"
  LDR R2, [FP, #-64]
  MOV R1, R2
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-60]
  STR R2, [FP, #-64]
  B .Lend_29
.Lelse_28:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var37
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
  @ bool False with name _var39
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
  @ int 13 with name _var41
  MOV R0, #13
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var40"
  LDR R1, [FP, #-76]
  @ then reg move
  MOV R1, R0
  @ then dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-76]
  B .Lend_35
.Lelse_34:
  @ else block
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool True with name _var42
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
  @ int 100 with name _var44
  MOV R0, #100
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 30 with name _var45
  MOV R1, #30
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ int 30 with name _var46
  MOV R2, #30
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ int 30 with name _var47
  MOV R3, #30
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ int 30 with name _var48
  MOV R4, #30
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ int 30 with name _var49
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
  @ loading var "_var43"
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
  B .Lend_38
.Lelse_37:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var43"
  LDR R1, [FP, #-84]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-84]
.Lend_38:
  @ loading var "_var40"
  LDR R1, [FP, #-76]
  @ else reg move
  @ loading var "_var43"
  LDR R0, [FP, #-84]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-84]
  STR R1, [FP, #-76]
.Lend_35:
  @ loading var "_var38"
  LDR R1, [FP, #-68]
  @ then reg move
  @ loading var "_var40"
  LDR R0, [FP, #-76]
  MOV R1, R0
  @ then dump regs
  STR R0, [FP, #-76]
  STR R1, [FP, #-68]
  B .Lend_32
.Lelse_31:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var38"
  LDR R1, [FP, #-68]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-68]
.Lend_32:
  @ loading var "_var34"
  LDR R1, [FP, #-60]
  @ else reg move
  @ loading var "_var38"
  LDR R0, [FP, #-68]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-68]
  STR R1, [FP, #-60]
.Lend_29:
  @ loading var "_var29"
  LDR R1, [FP, #-52]
  @ else reg move
  @ loading var "_var34"
  LDR R0, [FP, #-60]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-60]
  STR R1, [FP, #-52]
.Lend_26:
  @ loading var "_var25"
  LDR R1, [FP, #-44]
  @ else reg move
  @ loading var "_var29"
  LDR R0, [FP, #-52]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-52]
  STR R1, [FP, #-44]
.Lend_23:
  @ loading var "_var21"
  LDR R1, [FP, #-36]
  @ else reg move
  @ loading var "_var25"
  LDR R0, [FP, #-44]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-44]
  STR R1, [FP, #-36]
.Lend_20:
  @ loading var "_var18"
  LDR R1, [FP, #-28]
  @ else reg move
  @ loading var "_var21"
  LDR R0, [FP, #-36]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-36]
  STR R1, [FP, #-28]
.Lend_17:
  @ loading var "_var16"
  LDR R1, [FP, #-20]
  @ then reg move
  @ loading var "_var18"
  LDR R0, [FP, #-28]
  MOV R1, R0
  @ then dump regs
  STR R0, [FP, #-28]
  STR R1, [FP, #-20]
  B .Lend_14
.Lelse_13:
  @ else block
  @ List []
  MOV R0, #0
  @ loading var "_var16"
  LDR R1, [FP, #-20]
  @ else reg move
  MOV R1, R0
  @ else dump regs
  STMFD SP!, {R0}
  STR R1, [FP, #-20]
.Lend_14:
  @ loading var "_var13"
  LDR R2, [FP, #-12]
  @ else reg move
  @ loading var "_var16"
  LDR R0, [FP, #-20]
  MOV R2, R0
  @ else dump regs
  STR R0, [FP, #-20]
  STR R2, [FP, #-12]
.Lend_11:
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
  @ checks if x is atom
  @ number has mask 00001
  @ number has tag 00000
   AND R0, R0, #1
   CMP R0, #0
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
numberQM:
  @ def:  number? x
  @ checks if x is number
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
  @ untagging int, reg R0
   LSR R0, R0, #2
   BL print_int
   BX LR
  
print_bool_c_wrapper:
  @ def:  print_bool_c_wrapper
  @ body
  @ untagging int, reg R0
   LSR R0, R0, #3
   BL print_bool
   BX LR
  

