  @ global functions
  .global scheme_entry
  @ declarations of C functions
  @ declarations of global functions
scheme_entry:
  @MOV SP, #0x02300000
  @LSL SP, #4
  @ADD SP, SP, #0xF0000
  @LSL SP, #8
  @ADD SP, SP, #0xFF00
  @LSL SP, #8
  @ADD SP, SP, #0xFD
  ldr R0, =__end__
  bl print_int
  bx lr
  @MOV SP, #0x02400000

  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function dtcm-stack-size
  @ preparing arg _var0
  @ int 0 with name _var0
  MOV R0, #0
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL dtcm_stack_size
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function dtcm-stack-size
  MOV R0, R1
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
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
  @ preparing arg _var4
  @ int 1 with name _var3
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
  @ int 1 with name _var6
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
  @ int 1 with name _var9
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var8"
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
  @ preparing arg _var12
  @ int 1 with name _var11
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
  @ loading var "_var8"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var13"
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
  @ int 1 with name _var14
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
  @ int 1 with name _var17
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var16"
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
  @ preparing arg _var20
  @ int 1 with name _var19
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
  @ loading var "_var16"
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
  @ int 5 with name _var23
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
  @ int 1 with name _var24
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
  @ int 1 with name _var27
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var26"
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
  @ preparing arg _var29
  @ int 1 with name _var28
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
  @ loading var "_var26"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var31"
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
  @ int 2 with name _var32
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 1 with name _var33
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
  @ int 1 with name _var36
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var35"
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
  @ preparing arg _var37
  @ int 3 with name _var37
  MOV R0, #3
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var38
  @ int 2 with name _var38
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
  @ int 2 with name _var41
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var40"
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
  @ preparing arg _var42
  @ bool False with name _var42
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
  @ loading var "_var40"
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
  @ loading var "_var35"
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var40"
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
  @ bool False with name _var44
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
  @ int 10 with name _var46
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var45"
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
  @ bool True with name _var47
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
  @ bool False with name _var49
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
  @ int 11 with name _var51
  MOV R0, #11
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var50"
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
  @ bool True with name _var52
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
  @ preparing arg _var54
  @ int 32 with name _var54
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
  @ loading var "_var53"
  STMFD SP!, {R1}
  LDR R1, [FP, #-36]
  @ then reg move
  @ loading var "_var55"
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
  @ bool True with name _var56
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
  @ preparing arg _var58
  @ int 1 with name _var58
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
  @ loading var "_var57"
  STMFD SP!, {R1}
  LDR R1, [FP, #-44]
  @ then reg move
  @ loading var "_var59"
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
  @ bool True with name _var60
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
  @ preparing arg _var62
  @ int 1 with name _var62
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ preparing arg _var63
  @ int 2 with name _var63
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
  @ loading var "_var61"
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
  @ bool True with name _var65
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
  @ preparing arg _var67
  @ bool True with name _var67
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
  @ loading var "_var66"
  STMFD SP!, {R1}
  LDR R1, [FP, #-60]
  @ then reg move
  @ loading var "_var68"
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
  @ bool True with name _var69
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
  @ bool False with name _var71
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
  @ int 13 with name _var73
  MOV R0, #13
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ loading var "_var72"
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
  @ bool True with name _var74
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
  @ int 100 with name _var76
  MOV R0, #100
  @ tagging int, reg R0
  LSL R0, R0, #2
  ORR R0, R0, #2
  @ int 30 with name _var77
  MOV R1, #30
  @ tagging int, reg R1
  LSL R1, R1, #2
  ORR R1, R1, #2
  @ int 30 with name _var78
  MOV R2, #30
  @ tagging int, reg R2
  LSL R2, R2, #2
  ORR R2, R2, #2
  @ int 30 with name _var79
  MOV R3, #30
  @ tagging int, reg R3
  LSL R3, R3, #2
  ORR R3, R3, #2
  @ int 30 with name _var80
  MOV R4, #30
  @ tagging int, reg R4
  LSL R4, R4, #2
  ORR R4, R4, #2
  @ int 30 with name _var81
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
  @ loading var "_var75"
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
  @ loading var "_var75"
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
  @ loading var "_var72"
  LDR R1, [FP, #-76]
  @ else reg move
  @ loading var "_var75"
  LDR R0, [FP, #-84]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-84]
  STR R1, [FP, #-76]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-76
.Lend_53:
  @ loading var "_var70"
  LDR R1, [FP, #-68]
  @ then reg move
  @ loading var "_var72"
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
  @ loading var "_var70"
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
  @ loading var "_var66"
  LDR R1, [FP, #-60]
  @ else reg move
  @ loading var "_var70"
  LDR R0, [FP, #-68]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-68]
  STR R1, [FP, #-60]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-60
.Lend_47:
  @ loading var "_var61"
  LDR R1, [FP, #-52]
  @ else reg move
  @ loading var "_var66"
  LDR R0, [FP, #-60]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-60]
  STR R1, [FP, #-52]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-52
.Lend_44:
  @ loading var "_var57"
  LDR R1, [FP, #-44]
  @ else reg move
  @ loading var "_var61"
  LDR R0, [FP, #-52]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-52]
  STR R1, [FP, #-44]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-44
.Lend_41:
  @ loading var "_var53"
  LDR R1, [FP, #-36]
  @ else reg move
  @ loading var "_var57"
  LDR R0, [FP, #-44]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-44]
  STR R1, [FP, #-36]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-36
.Lend_38:
  @ loading var "_var50"
  LDR R1, [FP, #-28]
  @ else reg move
  @ loading var "_var53"
  LDR R0, [FP, #-36]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-36]
  STR R1, [FP, #-28]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-28
.Lend_35:
  @ loading var "_var48"
  LDR R1, [FP, #-20]
  @ then reg move
  @ loading var "_var50"
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
  @ loading var "_var48"
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
  @ loading var "_var45"
  LDR R2, [FP, #-12]
  @ else reg move
  @ loading var "_var48"
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
  

