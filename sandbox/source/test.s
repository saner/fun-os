  @ global functions
  .global scheme_entry
  @ global constants
  @ interrups start
  REG_IME: .word 0x04000208
  REG_IE: .word 0x04000210
  REG_IF: .word 0x04000214
  REG_TM0_DAT: .word 0x04000100
  REG_TM0_CNT: .word 0x04000102
  INT_HAND_SHIFT: .word 0x3ffc
  INTERR_HANDLER: .word 0x0b003ffc
  TM0_EN: .word 0b11000011
  @ interrups end
  @ stack size for a process
  DATA_START: .word 0x0b000000
  STACK_SIZE: .word 15360
  @ declarations of C functions
  @ declarations of global functions
  .type scheme_entry, %function
scheme_entry:
  @ def:  scheme-entry mem-addr mem-size
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ all necessary data kept in dtcm
   LDR SL, DATA_START
  @ setting stack beginning to SP
   MOV SP, R0
   ADD SP, SP, R1
  @ setting heap beginning to SL
   STR R0, [SL]
  @ processes
   BL initialize_processes
  @ interrupts
   BL initialize_interrupts
  @ run code
   BL user_code
   scheme_entry_loop:
   B scheme_entry_loop
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type initialize_processes, %function
initialize_processes:
  @ def:  initialize-processes
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ there are no processes running
   MOV R0, #0
   STR R0, [SL, #4]
  @ active process no
   MOV R0, #-1
   STR R0, [SL, #8]
  @ set active proc list to all -1
   MOV R0, SL
   ADD R0, R0, #8
   MOV R2, #-1
   MOV R1, #1
   MOV R3, #4
   active_proc_loop:
   CMP R1, #100
   BGT active_proc_end
   STR R2, [R0, R3]
   ADD R1, R1, #1
   ADD R3, R3, #4
   B active_proc_loop
   active_proc_end:
  @ add idle process
   ADR R0, idle_process
   BL add_process
  @ start idle process
  @ idle proc number in R0
   BL enable_process
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type enable_process, %function
enable_process:
  @ def:  enable-process proc-no
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ change state to waiting
  @ set R1 to Waiting
   MOV R1, #2
  @ tag int
   LSL R1, #3
   ORR R1, R1, #2
   BL change_process_state
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type disable_process, %function
disable_process:
  @ def:  disable-process proc-no
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ change state to Blocked
  @ set R1 to Blocked
   MOV R1, #3
  @ tag int
   LSL R1, #3
   ORR R1, R1, #2
   BL change_process_state
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type change_process_state, %function
change_process_state:
  @ def:  change-process-state proc-no new-state
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ copy proc no
   MOV R9, R0
  @ copy state
   MOV R8, R1
  @ untag int
   LSR R8, #3
  @ find a PCB block
   MOV R0, SL
   ADD R0, R0, #8
   MOV R1, #1
   MOV R3, #4
   proc_pcb_loop:
   MUL R4, R1, R3
   LDR R2, [R0, R4]
   CMP R2, R9
   BEQ proc_pcb_found
   ADD R1, R1, #1
   B proc_pcb_loop
   proc_pcb_found:
  @ set PCB block
   MOV R0, SL
   ADD R0, R0, #408
   SUB R7, R1, #1
   MOV R3, #84
   MUL R6, R7, R3
   ADD R0, R0, R6
  @ setting proc state
  @ proc state, 1 - Running, 2 - Waiting, 3 - Blocked
   STR R8, [R0, #16]
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type add_process, %function
add_process:
  @ def:  add-process proc
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ save proc addr
   MOV R9, R0
  @ ERROR: - FIX THIS
  @ ERROR: problem when > 100 processes
  @ find a free PCB block
   MOV R0, SL
   ADD R0, R0, #8
   MOV R1, #1
   MOV R3, #4
   free_proc_pcb_loop:
   MUL R4, R1, R3
   LDR R2, [R0, R4]
   CMP R2, #-1
   BEQ free_proc_pcb_found
   ADD R1, R1, #1
   B free_proc_pcb_loop
   free_proc_pcb_found:
   free_proc_pcb_end:
  @ get a new process no
   LDR R5, [SL, #4]
  @ untag int
   LSR R5, #3
  @ increase
   ADD R5, R5, #1
  @ tag int
   LSL R5, #3
   ORR R5, R5, #2
  @ set a proc count
   STR R5, [SL, #4]
  @ set a new process block no
   STR R5, [R0, R4]
  @ set PCB block
   MOV R0, SL
   ADD R0, R0, #408
   SUB R8, R1, #1
   MOV R3, #84
   MUL R6, R8, R3
   ADD R0, R0, R6
  @ setting
  @ proc no
   STR R5, [R0, #4]
  @ proc priority
   MOV R2, #1
   STR R2, [R0, #8]
  @ proc address
   STR R9, [R0, #12]
  @ proc state, 1 - Running, 2 - Waiting, 3 - Blocked
   MOV R2, #3
  @ tag int
   LSL R2, #3
   ORR R2, R2, #2
   STR R2, [R0, #16]
  @ CPSR - System mode
   MOV R2, #0b11111
   STR R2, [R0, #20]
  @ reg block
   ADD R0, R0, #20
  @ SL reg
   STR SL, [R0, #44]
  @ SP reg
   LDR R7, [SL]
   LDR R6, STACK_SIZE
   MUL R6, R1, R6
   ADD R6, R6, R7
   STR R6, [R0, #56]
  @ FP reg
   STR R6, [R0, #48]
  @ PC reg
   ADD R8, R9, #4
   STR R8, [R0, #64]
  @ in R0 proc no passed to process
   STR R5, [R0, #4]
  @ other regs not set
  @ return proc no
   MOV R0, R5
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type remove_process, %function
remove_process:
  @ def:  remove-process proc-no
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ copy proc no
   MOV R9, R0
  @ find a PCB block
   MOV R0, SL
   ADD R0, R0, #8
   MOV R1, #1
   MOV R3, #4
   rem_proc_pcb_loop:
   MUL R4, R1, R3
   LDR R2, [R0, R4]
   CMP R2, R9
   BEQ rem_proc_pcb_found
   ADD R1, R1, #1
   B rem_proc_pcb_loop
   rem_proc_pcb_found:
  @ update process no
   LDR R5, [SL, #4]
  @ untag int
   LSR R5, #3
  @ decrease
   SUB R5, R5, #1
  @ tag int
   LSL R5, #3
   ORR R5, R5, #2
   STR R5, [SL, #4]
  @ free PCB block
   MOV R2, #-1
   STR R2, [R0, R4]
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type attt, %function
attt:
  @ define metal
  @ metal start
  @ prologue start
  STMFD SP!, {R0}
  MOV FP, SP
  @ prologue end
  @ process body: start
  @ int 1 with name _var0
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ int 2 with name _var1
  MOV R2, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ untagging int, reg R1
  LSR R1, R1, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  ADD R3, R1, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ process body: end
  @ epilog start
  @ estore process no
  MOV SP, FP
  LDMFD SP!, {R9}
  @ epilog end
  @ disable interrupts
  LDR R5, REG_IME
  MOV R6, #0
  LDR R7, [R5]
  STR R6, [R5]
  @ remove process
  @ it should be enough
  @ scheduler won't select this process
  @ because it is removed (no PCB)
  MOV R0, R9
  BL remove_process
  @ this process is running, set active process to -1
  @ next time scheduler runs, it will select another process
  MOV R8, #-1
  LDR R8, [SL, #8]
  @ restore interrupts
  LDR R5, REG_IME
  STR R7, [R5]
  @ enter infinite loop
  @ until scheduler removes this process
  sample_inf_loop0:
  B sample_inf_loop0
  @ process end
  @ metal end
  .type att2, %function
att2:
  @ define metal
  @ metal start
  @ prologue start
  STMFD SP!, {R0}
  MOV FP, SP
  @ prologue end
  @ process body: start
  @ int 1 with name _var3
  MOV R1, #1
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ int 2 with name _var4
  MOV R2, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ untagging int, reg R1
  LSR R1, R1, #3
  @ untagging int, reg R2
  LSR R2, R2, #3
  ADD R3, R1, R2
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ process body: end
  @ epilog start
  @ estore process no
  MOV SP, FP
  LDMFD SP!, {R9}
  @ epilog end
  @ disable interrupts
  LDR R5, REG_IME
  MOV R6, #0
  LDR R7, [R5]
  STR R6, [R5]
  @ remove process
  @ it should be enough
  @ scheduler won't select this process
  @ because it is removed (no PCB)
  MOV R0, R9
  BL remove_process
  @ this process is running, set active process to -1
  @ next time scheduler runs, it will select another process
  MOV R8, #-1
  LDR R8, [SL, #8]
  @ restore interrupts
  LDR R5, REG_IME
  STR R7, [R5]
  @ enter infinite loop
  @ until scheduler removes this process
  sample_inf_loop1:
  B sample_inf_loop1
  @ process end
  @ metal end
  .type sample_process, %function
sample_process:
  @ def:  sample-process no
  @ prologue start
   STMFD SP!, {R0}
   MOV FP, SP
  @ prologue end
  @ process body: start
  @ process body: end
  @ epilog start
  @ restore process no
   MOV SP, FP
   LDMFD SP!, {R9}
  @ epilog end
  @ disable interrupts
   LDR R5, REG_IME
   MOV R6, #0
   LDR R7, [R5]
   STR R6, [R5]
  @ remove process
  @ it should be enough
  @ scheduler won't select this process
  @ because it is removed (no PCB)
   MOV R0, R9
   BL remove_process
  @ this process is running, set active process to -1
  @ next time scheduler runs, it will select another process
   MOV R8, #-1
   LDR R8, [SL, #8]
  @ restore interrupts
   LDR R5, REG_IME
   STR R7, [R5]
  @ enter infinite loop
  @ until scheduler removes this process
   sample_inf_loop:
   B sample_inf_loop
  @ process end
  
  .type idle_process, %function
idle_process:
  @ def:  idle-process no
  @ process prologue
  @ process body: start
  @ process body: end
  @ enter infinite loop
   idle_inf_loop:
   B idle_inf_loop
  @ process end
  
  .type idle_process_2, %function
idle_process_2:
  @ def:  idle-process-2
   MOV R0, #0
   MOV R1, #11
   MOV R2, #21
   MOV R3, #31
   MOV R4, #41
   MOV R5, #51
   MOV R6, #61
   MOV R7, #71
   MOV R8, #81
   MOV R9, #91
   MOV R12, #121
   idle_2_inf_loop:
   B idle_2_inf_loop
  @ process end
  
  .type initialize_interrupts, %function
initialize_interrupts:
  @ def:  initialize-interrupts
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ interrupt start
  @ disable interrupts, REG_IME = 0
   LDR R5, REG_IME
   MOV R6, #0
   STR R6, [R5]
  @ set TM0 value
   LDR R5, REG_TM0_DAT
   MOV R6, #0
   STRH R6, [R5]
  @ set TM0 control
  @ enabled, irq, prescale 1024
   LDR R5, REG_TM0_CNT
   LDR R6, TM0_EN
   STRh R6, [R5]
  @ enable TM0
   LDR R5, REG_IE
   LDR R6, [R5]
   ORR R6, R6, #0b1000
   MOV R6, #0b1000
   STR R6, [R5]
  @ set interrupt handler
   LDR R5, INTERR_HANDLER
   ADR R6, interrupt_handler
   STR R6, [R5]
  @ enable interrupts, REG_IME = 1
   LDR R5, REG_IME
   MOV R6, #0b1
   STR R6, [R5]
  @ interrupt end
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type interrupt_handler, %function
interrupt_handler:
  @ def:  interrupt-handler
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
   LDR R5, REG_IF
   LDR R6, [R5]
   vblank:
   MOV R7, R6
   AND R7, R7, #0b0001
   CMP R7, #0b0001
   BNE timer
   MOV R8, #0b0001
   timer:
   MOV R7, R6
   AND R7, R7, #0b1000
   CMP R7, #0b1000
   BNE end
   ORR R8, R8, #0b1000
  @ low level operations
  @ reg r0-r3 not used later
  @ don't need to be saved on the stack
  @ select process no to run
  @ process no in R0
   MOV R0, #199
   BL print_int
   BL select_process
   MOV R9, R0
   LSR R0, #3
   BL print_int
   MOV R0, R9
   BL run_process
   end:
   STR R8, [R5]
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type run_process, %function
run_process:
  @ def:  run-process no
  @ it is called from interrupt handler
  @ copy active process to PCB block
   LDR R4, [SL, #8]
   CMP R4, #-1
  @ no active process, just load a new process
   BEQ run_process_load_proc
  @ find active proc PCB block
   ADD R1, SL, #8
   MOV R2, #0
   run_proc_active_pcb_find:
   ADD R1, R1, #4
   ADD R2, R2, #1
   LDR R3, [R1]
   CMP R4, R3
   BEQ run_proc_active_pcb_found
   B run_proc_active_pcb_find
   run_proc_active_pcb_found:
  @ active pcb found
   SUB R2, R2, #1
   MOV R3, #84
   MUL R1, R2, R3
   ADD R2, SL, #428
   ADD R2, R1, R2
  @ R2 points to CPSR positionin PCB
  @ save active process to pcb
  @ state -> Waiting
   MOV R1, #2
  @ tag int
   LSL R1, #3
   ORR R1, R1, #2
   STR R1, [R2, #-4]
  @ CPSR
   MRS R1, SPSR
   STR R1, [R2]
  @ R0
   LDR R1, [SP, #36]
   STR R1, [R2, #4]
  @ R1
   LDR R1, [SP, #40]
   STR R1, [R2, #8]
  @ R2
   LDR R1, [SP, #44]
   STR R1, [R2, #12]
  @ R3
   LDR R1, [SP, #48]
   STR R1, [R2, #16]
  @ R4
   LDR R1, [SP, #8]
   STR R1, [R2, #20]
  @ R5
   LDR R1, [SP, #12]
   STR R1, [R2, #24]
  @ R6
   LDR R1, [SP, #16]
   STR R1, [R2, #28]
  @ R7
   LDR R1, [SP, #20]
   STR R1, [R2, #32]
  @ R8
   LDR R1, [SP, #24]
   STR R1, [R2, #36]
  @ R9
   LDR R1, [SP, #28]
   STR R1, [R2, #40]
  @ starting from R10 special registers
  @ R10 - SL
   LDR R1, [SP, #4]
   STR R1, [R2, #44]
  @ R11 - FP
   LDR R1, [SP]
   STR R1, [R2, #48]
  @ R12 - IP
   LDR R1, [SP, #52]
   STR R1, [R2, #52]
  @ R13 - SP
  @ go back to SYSTEM mode
  @ to access process SP
   MRS R3, CPSR
   ORR R1, R3, #0b11111
   MSR CPSR, R1
   MOV R1, SP
   MSR CPSR, R3
   STR R1, [R2, #56]
  @ R14 - LR
   MRS R3, CPSR
   ORR R1, R3, #0b11111
   MSR CPSR, R1
   MOV R1, LR
   MSR CPSR, R3
   STR R1, [R2, #60]
  @ R15 - PC
   LDR R1, [SP, #56]
   STR R1, [R2, #64]
   run_process_load_proc:
  @ find new proc PCB block
   ADD R1, SL, #8
   MOV R2, #0
   run_proc_new_pcb_find:
   ADD R1, R1, #4
   ADD R2, R2, #1
   LDR R3, [R1]
   CMP R0, R3
   BEQ run_proc_new_pcb_found
   B run_proc_new_pcb_find
   run_proc_new_pcb_found:
  @ new pcb found
   SUB R2, R2, #1
   MOV R3, #84
   MUL R1, R2, R3
   ADD R2, SL, #428
   ADD R2, R1, R2
  @ R2 points to CPSR positionin PCB
  @ load new process from pcb
  @ state -> Running
   MOV R1, #1
  @ tag int
   LSL R1, #3
   ORR R1, R1, #2
   STR R1, [R2, #-4]
  @ CPSR
   LDR R1, [R2]
   MSR SPSR, R1
  @ R0
   LDR R1, [R2, #4]
   STR R1, [SP, #36]
  @ R1
   LDR R1, [R2, #8]
   STR R1, [SP, #40]
  @ R2
   LDR R1, [R2, #12]
   STR R1, [SP, #44]
  @ R3
   LDR R1, [R2, #16]
   STR R1, [SP, #48]
  @ R4
   LDR R1, [R2, #20]
   STR R1, [SP, #8]
  @ R5
   LDR R1, [R2, #24]
   STR R1, [SP, #12]
  @ R6
   LDR R1, [R2, #28]
   STR R1, [SP, #16]
  @ R7
   LDR R1, [R2, #32]
   STR R1, [SP, #20]
  @ R8
   LDR R1, [R2, #36]
   STR R1, [SP, #24]
  @ R9
   LDR R1, [R2, #40]
   STR R1, [SP, #28]
  @ starting from R10 special registers
  @ R10 - SL
   LDR R1, [R2, #44]
   STR R1, [SP, #4]
  @ R11 - FP
   LDR R1, [R2, #48]
   STR R1, [SP]
  @ R12 - IP
   LDR R1, [R2, #52]
   STR R1, [SP, #52]
  @ R13 - SP
  @ go back to SYSTEM mode
  @ to set process SP
   MRS R3, CPSR
   ORR R1, R3, #0b11111
   MSR CPSR, R1
   LDR R1, [R2, #56]
   MOV SP, R1
   MSR CPSR, R3
  @ R14 - LR
   MRS R3, CPSR
   ORR R1, R3, #0b11111
   MSR CPSR, R1
   LDR R1, [R2, #60]
   MOV LR, R1
   MSR CPSR, R3
  @ R15 - PC
   LDR R1, [R2, #64]
   STR R1, [SP, #56]
  @ set active process no
   STR R0, [SL, #8]
   run_proc_end:
  
  .type select_process, %function
select_process:
  @ def:  select-process
  @ it is called from interrupt handler
  @ prologue start
   STMFD SP!, {LR}
   STMFD SP!, {R4, R5, R6, R7, R8, R9}
   STMFD SP!, {SL}
   STMFD SP!, {FP}
   MOV FP, SP
  @ prologue end
  @ check if there is a running process
   LDR R0, [SL, #8]
   CMP R0, #-1
   BEQ sele_proc_sel_no_running
   sele_proc_sel_is_running:
  @ find proc PCB block
   ADD R1, SL, #8
   MOV R2, #0
   sele_proc_sel_find:
   ADD R1, R1, #4
   ADD R2, R2, #1
   LDR R3, [R1]
   CMP R0, R3
   BEQ sele_proc_next_find
   B sele_proc_sel_find
   sele_proc_sel_no_running:
   ADD R1, SL, #8
   MOV R2, #0
   sele_proc_next_find:
  @ current process PCB found
  @ find next waiting process
   ADD R1, R1, #4
   ADD R2, R2, #1
   CMP R2, #100
   BGT sele_proc_next_not_found
   LDR R3, [R1]
   CMP R3, #-1
   BEQ sele_proc_next_find
  @ we have PCB block
  @ check if a state of the process is waiting
  @ get PCB block
   ADD R4, SL, #408
   SUB R5, R2, #1
   MOV R3, #84
   MUL R6, R5, R3
   ADD R4, R4, R6
   LDR R7, [R4, #16]
   MOV R8, #3
  @ tag int
   LSL R8, #3
   ORR R8, R8, #2
   CMP R7, R8
   BNE sele_proc_next_found
   B sele_proc_next_find
   sele_proc_next_not_found:
  @ did not find any process
  @ start searching from the beginning
   B sele_proc_sel_no_running
   sele_proc_next_found:
   LDR R0, [R4, #4]
  @ epilog start
   MOV SP, FP
   LDMFD SP!, {FP}
   LDMFD SP!, {SL}
   LDMFD SP!, {R4, R5, R6, R7, R8, R9}
   LDMFD SP!, {LR}
   BX LR
  @ epilog end
  
  .type alloc_mem, %function
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
  .type make_vector, %function
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
  
  .type vector_ref, %function
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
  
  .type vector_setEM, %function
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
  
  .type vector_length, %function
vector_length:
  @ def:  vector-length v
  @ returns length of a vector
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   LDR R0, [R0]
   BX LR
  
  .type vectorQM, %function
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
  .type cons, %function
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
  
  .type pairQM, %function
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
  
  .type car, %function
car:
  @ def:  car c
  @ returns car of cons
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   LDR R0, [R0]
   BX LR
  
  .type cdr, %function
cdr:
  @ def:  cdr c
  @ returns cdr of cons
  @ untag
   AND R0, R0, #0xFFFFFFF8
  @ return
   ADD R0, R0, #4
   LDR R0, [R0]
   BX LR
  
  .type user_code, %function
user_code:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function enable-process
  @ preparing arg _var6
  @ calling function add-process
  @ preparing arg first-proc
  @ !!!!!!!!! - ERROR : moveRegToStack: no var in reg: R0
  ADR R0, first_proc
  @ loading to reg arg 1
  @ !!!!!!!!! - ERROR : loadVarToRegister: var was not found in state
  BL add_process
  MOV R1, R0
  @ call end function add-process
  @ loading to reg arg 1
  MOV R0, R1
  STMFD SP!, {R0}
  BL enable_process
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function enable-process
  @ calling function enable-process
  @ preparing arg _var8
  @ calling function add-process
  @ preparing arg second-proc
  STMFD SP!, {R0}
  ADR R0, second_proc
  @ loading to reg arg 1
  @ !!!!!!!!! - ERROR : loadVarToRegister: var was not found in state
  STMFD SP!, {R1}
  BL add_process
  MOV R2, R0
  LDMFD SP!, {R1}
  @ call end function add-process
  @ loading to reg arg 1
  MOV R0, R2
  STMFD SP!, {R0, R1}
  BL enable_process
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function enable-process
  MOV R0, R2
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  .type first_proc, %function
first_proc:
  @ define metal
  @ metal start
  @ prologue start
  STMFD SP!, {R0}
  MOV FP, SP
  @ prologue end
  @ process body: start
  @ calling function print_int_c_wrapper
  @ preparing arg _var10
  @ int 101 with name _var10
  MOV R0, #101
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ process body: end
  @ epilog start
  @ estore process no
  MOV SP, FP
  LDMFD SP!, {R9}
  @ epilog end
  @ disable interrupts
  LDR R5, REG_IME
  MOV R6, #0
  LDR R7, [R5]
  STR R6, [R5]
  @ remove process
  @ it should be enough
  @ scheduler won't select this process
  @ because it is removed (no PCB)
  MOV R0, R9
  BL remove_process
  @ this process is running, set active process to -1
  @ next time scheduler runs, it will select another process
  MOV R8, #-1
  LDR R8, [SL, #8]
  @ restore interrupts
  LDR R5, REG_IME
  STR R7, [R5]
  @ enter infinite loop
  @ until scheduler removes this process
  sample_inf_loop2:
  B sample_inf_loop2
  @ process end
  @ metal end
  .type second_proc, %function
second_proc:
  @ define metal
  @ metal start
  @ prologue start
  STMFD SP!, {R0}
  MOV FP, SP
  @ prologue end
  @ process body: start
  @ calling function print_int_c_wrapper
  @ preparing arg _var12
  @ int 102 with name _var12
  MOV R0, #102
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading to reg arg 1
  STMFD SP!, {R0}
  BL print_int_c_wrapper
  MOV R1, R0
  LDMFD SP!, {R0}
  @ call end function print_int_c_wrapper
  @ process body: end
  @ epilog start
  @ estore process no
  MOV SP, FP
  LDMFD SP!, {R9}
  @ epilog end
  @ disable interrupts
  LDR R5, REG_IME
  MOV R6, #0
  LDR R7, [R5]
  STR R6, [R5]
  @ remove process
  @ it should be enough
  @ scheduler won't select this process
  @ because it is removed (no PCB)
  MOV R0, R9
  BL remove_process
  @ this process is running, set active process to -1
  @ next time scheduler runs, it will select another process
  MOV R8, #-1
  LDR R8, [SL, #8]
  @ restore interrupts
  LDR R5, REG_IME
  STR R7, [R5]
  @ enter infinite loop
  @ until scheduler removes this process
  sample_inf_loop3:
  B sample_inf_loop3
  @ process end
  @ metal end
  .type test_vector_comp, %function
test_vector_comp:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ vector constructor
  @ let block start
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var14
  @ int 3 with name _var14
  MOV R0, #3
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
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var16
  @ int 0 with name _var16
  MOV R3, #0
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var17
  @ int 1 with name _var17
  MOV R4, #1
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R3
  @ loading to reg arg 1
  @ loading var "_var17"
  LDR R2, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL vector_setEM
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var19
  @ int 1 with name _var19
  MOV R4, #1
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg vec_1
  @ vector constructor
  @ let block start
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var20
  @ int 2 with name _var20
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 1
  STMFD SP!, {R0}
  MOV R0, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL make_vector
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function make-vector
  MOV R5, R4
  @ let block var def end
  @ calling function vector-set!
  @ preparing arg vec_1
  @ preparing arg _var22
  @ int 0 with name _var22
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ preparing arg _var23
  @ int 10 with name _var23
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
  STR R2, [FP, #-4]
  MOV R2, R7
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg vec_1
  @ preparing arg _var25
  @ int 1 with name _var25
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var26
  @ int 20 with name _var26
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
  @ let block end
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading var "vec_0"
  STMFD SP!, {R0}
  LDR R0, [FP, #-20]
  @ loading to reg arg 2
  @ loading var "_var19"
  STMFD SP!, {R1}
  LDR R1, [FP, #-16]
  @ loading to reg arg 1
  @ loading var "vec_1"
  STMFD SP!, {R2}
  LDR R2, [FP, #-52]
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var29
  @ int 2 with name _var29
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var32
  @ int 2 with name _var30
  MOV R6, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ int 3 with name _var31
  MOV R7, #3
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  @ untagging int, reg R6
  LSR R6, R6, #3
  @ untagging int, reg R7
  LSR R7, R7, #3
  ADD R8, R6, R7
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ tagging int, reg R7
  LSL R7, R7, #3
  ORR R7, R7, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STR R1, [FP, #-16]
  MOV R1, R5
  @ loading to reg arg 1
  STR R2, [FP, #-52]
  MOV R2, R8
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ let block end
  MOV R5, R0
  @ let block var def end
  @ calling function print_int_c_wrapper
  @ preparing arg _var35
  @ calling function vector-ref
  @ preparing arg v
  @ preparing arg _var34
  @ int 0 with name _var34
  MOV R8, #0
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-20]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R8
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var35"
  STMFD SP!, {R0}
  LDR R0, [FP, #-76]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var40
  @ calling function vector-ref
  @ preparing arg _var38
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-80]
  @ preparing arg _var37
  @ int 1 with name _var37
  MOV R8, #1
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-76]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R8
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  @ preparing arg _var39
  @ int 0 with name _var39
  MOV R5, #0
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var38"
  STR R0, [FP, #-80]
  LDR R0, [FP, #-92]
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var40"
  STR R0, [FP, #-92]
  LDR R0, [FP, #-100]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var43
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-80]
  @ preparing arg _var42
  @ int 2 with name _var42
  MOV R8, #2
  @ tagging int, reg R8
  LSL R8, R8, #3
  ORR R8, R8, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-100]
  MOV R0, R5
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R8
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var43"
  STR R0, [FP, #-80]
  LDR R0, [FP, #-112]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ let block end
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  .type test_vector_literal, %function
test_vector_literal:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ vector constructor
  @ let block start
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var45
  @ int 5 with name _var45
  MOV R0, #5
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
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var47
  @ int 0 with name _var47
  MOV R3, #0
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var48
  @ int 1 with name _var48
  MOV R4, #1
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R3
  @ loading to reg arg 1
  @ loading var "_var48"
  LDR R2, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL vector_setEM
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var50
  @ int 1 with name _var50
  MOV R4, #1
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var51
  @ int 2 with name _var51
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  @ loading var "_var50"
  STMFD SP!, {R1}
  LDR R1, [FP, #-16]
  @ loading to reg arg 1
  STR R2, [FP, #-4]
  MOV R2, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_setEM
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-set!
  @ calling function vector-set!
  @ preparing arg vec_0
  @ preparing arg _var53
  @ int 2 with name _var53
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var54
  @ int 3 with name _var54
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  STR R1, [FP, #-16]
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
  @ preparing arg vec_0
  @ preparing arg _var56
  @ int 3 with name _var56
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var57
  @ int 4 with name _var57
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
  @ preparing arg vec_0
  @ preparing arg _var59
  @ int 4 with name _var59
  MOV R5, #4
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var60
  @ int 5 with name _var60
  MOV R6, #5
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
  @ let block end
  MOV R5, R0
  @ let block var def end
  @ calling function print_int_c_wrapper
  @ preparing arg _var63
  @ calling function vector-ref
  @ preparing arg v
  @ preparing arg _var62
  @ int 0 with name _var62
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
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
  @ loading var "_var63"
  STMFD SP!, {R0}
  LDR R0, [FP, #-68]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var66
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-72]
  @ preparing arg _var65
  @ int 1 with name _var65
  MOV R6, #1
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-68]
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
  @ loading var "_var66"
  STR R0, [FP, #-72]
  LDR R0, [FP, #-84]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var69
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-72]
  @ preparing arg _var68
  @ int 2 with name _var68
  MOV R6, #2
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
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var69"
  STR R0, [FP, #-72]
  LDR R0, [FP, #-96]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var72
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-72]
  @ preparing arg _var71
  @ int 3 with name _var71
  MOV R6, #3
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-96]
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
  @ loading var "_var72"
  STR R0, [FP, #-72]
  LDR R0, [FP, #-108]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var75
  @ calling function vector-ref
  @ preparing arg v
  @ loading var "v"
  LDR R5, [FP, #-72]
  @ preparing arg _var74
  @ int 4 with name _var74
  MOV R6, #4
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
  @ loading var "_var75"
  STR R0, [FP, #-72]
  LDR R0, [FP, #-120]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ let block end
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  .type test_vector_simp, %function
test_vector_simp:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ vector construct
  @ let block start
  @ let block var def start
  @ calling function make-vector
  @ preparing arg _var77
  @ int 4 with name _var77
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
  @ calling function vector-set!
  @ preparing arg vec01
  @ preparing arg _var79
  @ int 0 with name _var79
  MOV R3, #0
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var80
  @ int 3 with name _var80
  MOV R4, #3
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 2
  STMFD SP!, {R1}
  MOV R1, R3
  @ loading to reg arg 1
  @ loading var "_var80"
  LDR R2, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL vector_setEM
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function vector-set!
  @ let block end
  MOV R4, R0
  @ let block var def end
  @ calling function print_int_c_wrapper
  @ preparing arg _var83
  @ calling function vector-ref
  @ preparing arg vec01
  @ preparing arg _var82
  @ int 0 with name _var82
  MOV R5, #0
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading to reg arg 1
  STMFD SP!, {R1}
  MOV R1, R5
  STMFD SP!, {R0, R1, R2, R3}
  BL vector_ref
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function vector-ref
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var83"
  STMFD SP!, {R0}
  LDR R0, [FP, #-24]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ let block end
  MOV R0, R4
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  .type test_list_exp_2, %function
test_list_exp_2:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ calling function cons
  @ preparing arg _var85
  @ int 1 with name _var85
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var94
  @ calling function cons
  @ preparing arg _var90
  @ calling function cons
  @ preparing arg _var86
  @ int 10 with name _var86
  MOV R1, #10
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ preparing arg _var89
  @ calling function cons
  @ preparing arg _var87
  @ int 20 with name _var87
  MOV R2, #20
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ preparing arg _var88
  @ empty list with name _var88
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
  @ loading var "_var86"
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
  @ preparing arg _var93
  @ calling function cons
  @ preparing arg _var91
  @ int 3 with name _var91
  MOV R3, #3
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var92
  @ empty list with name _var92
  MOV R4, #0
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STR R0, [FP, #-8]
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var92"
  STMFD SP!, {R1}
  LDR R1, [FP, #-20]
  STMFD SP!, {R0, R1, R2}
  BL cons
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cons
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R2
  @ loading to reg arg 1
  STR R1, [FP, #-20]
  MOV R1, R3
  STMFD SP!, {R0, R1}
  BL cons
  MOV R2, R0
  LDMFD SP!, {R0, R1}
  @ call end function cons
  @ loading to reg arg 2
  @ loading var "_var85"
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
  @ preparing arg _var99
  @ calling function car
  @ preparing arg _var98
  @ calling function cdr
  @ preparing arg _var97
  @ calling function car
  @ preparing arg _var96
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
  BL car
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function car
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
  @ let block end
  MOV R0, R3
  @ body end
  MOV SP, FP
  LDMFD SP!, {FP}
  LDMFD SP!, {SL}
  LDMFD SP!, {R4, R5, R6, R7, R8, R9}
  LDMFD SP!, {LR}
  BX LR
  .type test_list_exp, %function
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
  @ preparing arg _var101
  @ int 1 with name _var101
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var106
  @ calling function cons
  @ preparing arg _var102
  @ int 2 with name _var102
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ preparing arg _var105
  @ calling function cons
  @ preparing arg _var103
  @ int 3 with name _var103
  MOV R2, #3
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ preparing arg _var104
  @ empty list with name _var104
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
  @ loading var "_var102"
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
  @ loading var "_var101"
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
  @ preparing arg _var110
  @ calling function cdr
  @ preparing arg _var109
  @ calling function cdr
  @ preparing arg _var108
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
  .type test_cons_complex, %function
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
  @ preparing arg _var112
  @ int 1 with name _var112
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var115
  @ calling function cons
  @ preparing arg _var113
  @ int 2 with name _var113
  MOV R1, #2
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ preparing arg _var114
  @ int 3 with name _var114
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
  @ loading var "_var112"
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
  @ preparing arg _var118
  @ calling function cdr
  @ preparing arg _var117
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
  .type test_let_2, %function
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
  @ preparing arg _var120
  @ int 4 with name _var120
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
  @ preparing arg _var122
  @ int 5 with name _var122
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
  @ preparing arg _var124
  @ int 0 with name _var124
  MOV R5, #0
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var125
  @ int 0 with name _var125
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
  @ preparing arg _var127
  @ int 1 with name _var127
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var128
  @ int 1 with name _var128
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
  @ preparing arg _var130
  @ int 2 with name _var130
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var131
  @ int 2 with name _var131
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
  @ preparing arg _var133
  @ int 3 with name _var133
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var134
  @ int 3 with name _var134
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
  @ preparing arg _var136
  @ int 0 with name _var136
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ preparing arg _var137
  @ int 10 with name _var137
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
  @ preparing arg _var139
  @ int 1 with name _var139
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var140
  @ int 20 with name _var140
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
  @ preparing arg _var142
  @ int 2 with name _var142
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var143
  @ int 30 with name _var143
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
  @ preparing arg _var145
  @ int 3 with name _var145
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var146
  @ int 40 with name _var146
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
  @ preparing arg _var148
  @ int 3 with name _var148
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
  @ preparing arg _var150
  @ int 2 with name _var150
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
  @ loading var "_var149"
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
  .type test_let, %function
test_let:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ let block start
  @ let block var def start
  @ int 1 with name _var153
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  MOV R1, R0
  @ let block var def end
  @ let block var def start
  @ int 5 with name _var154
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
  .type test_vector_init, %function
test_vector_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-vector
  @ preparing arg _var157
  @ calling function make-vector
  @ preparing arg _var156
  @ int 10 with name _var156
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
  @ preparing arg _var160
  @ calling function cons
  @ preparing arg _var158
  @ int 99 with name _var158
  MOV R2, #99
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ preparing arg _var159
  @ int 9 with name _var159
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
  @ preparing arg _var162
  @ calling function make-vector
  @ preparing arg _var161
  @ int 5 with name _var161
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
  @ loading var "_var157"
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
  .type test_vector, %function
test_vector:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var164
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
  @ preparing arg _var166
  @ int 6 with name _var166
  MOV R4, #6
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var167
  @ int 1 with name _var167
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 2
  @ loading var "_var166"
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
  @ preparing arg _var169
  @ int 7 with name _var169
  MOV R4, #7
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var170
  @ int 2 with name _var170
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 3
  @ loading to reg arg 2
  @ loading var "_var169"
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
  @ preparing arg _var172
  @ int 8 with name _var172
  MOV R5, #8
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var173
  @ int 3 with name _var173
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
  @ preparing arg _var175
  @ int 9 with name _var175
  MOV R5, #9
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var176
  @ int 4 with name _var176
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
  @ preparing arg _var178
  @ int 0 with name _var178
  MOV R6, #0
  @ tagging int, reg R6
  LSL R6, R6, #3
  ORR R6, R6, #2
  @ preparing arg _var179
  @ int 10 with name _var179
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
  @ preparing arg _var181
  @ int 1 with name _var181
  MOV R5, #1
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var182
  @ int 20 with name _var182
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
  @ preparing arg _var184
  @ int 2 with name _var184
  MOV R5, #2
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var185
  @ int 30 with name _var185
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
  @ preparing arg _var187
  @ int 3 with name _var187
  MOV R5, #3
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var188
  @ int 40 with name _var188
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
  @ preparing arg _var190
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
  @ loading var "_var190"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-108]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var193
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var192
  @ int 6 with name _var192
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
  @ loading var "_var193"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-120]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var196
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var195
  @ int 7 with name _var195
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
  @ loading var "_var196"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-132]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var199
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var198
  @ int 8 with name _var198
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
  @ loading var "_var199"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-144]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var202
  @ calling function vector-ref
  @ preparing arg v1
  @ loading var "v1"
  LDR R5, [FP, #-4]
  @ preparing arg _var201
  @ int 9 with name _var201
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
  @ loading var "_var202"
  STR R0, [FP, #-4]
  LDR R0, [FP, #-156]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var205
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var204
  @ int 0 with name _var204
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
  @ loading var "_var205"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-168]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var208
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var207
  @ int 1 with name _var207
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
  @ loading var "_var208"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-180]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var211
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var210
  @ int 2 with name _var210
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
  @ loading var "_var211"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-192]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var214
  @ calling function vector-ref
  @ preparing arg v2
  @ loading var "v2"
  LDR R5, [FP, #-24]
  @ preparing arg _var213
  @ int 3 with name _var213
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
  @ loading var "_var214"
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
  .type test_cons_init, %function
test_cons_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function test-cons
  @ preparing arg _var218
  @ calling function cons
  @ preparing arg _var216
  @ int 1 with name _var216
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var217
  @ int 2 with name _var217
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
  @ preparing arg _var221
  @ calling function cons
  @ preparing arg _var219
  @ int 3 with name _var219
  MOV R3, #3
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var220
  @ int 4 with name _var220
  MOV R4, #4
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var220"
  STMFD SP!, {R1}
  LDR R1, [FP, #-4]
  STMFD SP!, {R0, R1, R2}
  BL cons
  MOV R3, R0
  LDMFD SP!, {R0, R1, R2}
  @ call end function cons
  @ preparing arg _var224
  @ calling function cons
  @ preparing arg _var222
  @ int 5 with name _var222
  MOV R4, #5
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var223
  @ int 6 with name _var223
  MOV R5, #6
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var222"
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
  @ preparing arg _var227
  @ calling function cons
  @ preparing arg _var225
  @ int 7 with name _var225
  MOV R5, #7
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var226
  @ int 8 with name _var226
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
  @ loading var "_var224"
  LDR R2, [FP, #-24]
  @ loading to reg arg 1
  @ loading var "_var227"
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
  .type test_cons, %function
test_cons:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var229
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
  @ loading var "_var229"
  STMFD SP!, {R0}
  LDR R0, [FP, #-4]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var231
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
  @ loading var "_var231"
  STR R0, [FP, #-8]
  LDR R0, [FP, #-16]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var233
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
  @ preparing arg _var235
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
  @ loading var "_var235"
  STR R0, [FP, #-24]
  LDR R0, [FP, #-32]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var237
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
  @ preparing arg _var239
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
  @ loading var "_var239"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-48]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var241
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
  @ preparing arg _var243
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
  @ loading var "_var243"
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
  .type test_cons_2_init, %function
test_cons_2_init:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ calling function print_int_c_wrapper
  @ preparing arg _var248
  @ calling function car
  @ preparing arg _var247
  @ calling function cons
  @ preparing arg _var245
  @ int 1 with name _var245
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var246
  @ int 2 with name _var246
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
  @ preparing arg _var253
  @ calling function car
  @ preparing arg _var252
  @ calling function cons
  @ preparing arg _var250
  @ int 10 with name _var250
  MOV R3, #10
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ preparing arg _var251
  @ int 20 with name _var251
  MOV R4, #20
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  STMFD SP!, {R0}
  MOV R0, R3
  @ loading to reg arg 1
  @ loading var "_var251"
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
  @ preparing arg _var258
  @ calling function car
  @ preparing arg _var257
  @ calling function cons
  @ preparing arg _var255
  @ int 11 with name _var255
  MOV R4, #11
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ preparing arg _var256
  @ int 21 with name _var256
  MOV R5, #21
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  STMFD SP!, {R4}
  @ loading to reg arg 2
  @ loading var "_var255"
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
  @ loading var "_var257"
  STR R0, [FP, #-32]
  LDR R0, [FP, #-40]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var258"
  STR R0, [FP, #-40]
  LDR R0, [FP, #-44]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var263
  @ calling function car
  @ preparing arg _var262
  @ calling function cons
  @ preparing arg _var260
  @ int 12 with name _var260
  MOV R5, #12
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var261
  @ int 22 with name _var261
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
  @ loading var "_var262"
  STMFD SP!, {R0}
  LDR R0, [FP, #-56]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var263"
  STR R0, [FP, #-56]
  LDR R0, [FP, #-64]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var268
  @ calling function car
  @ preparing arg _var267
  @ calling function cons
  @ preparing arg _var265
  @ int 13 with name _var265
  MOV R5, #13
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var266
  @ int 23 with name _var266
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
  @ loading var "_var267"
  STMFD SP!, {R0}
  LDR R0, [FP, #-76]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var268"
  STR R0, [FP, #-76]
  LDR R0, [FP, #-84]
  STMFD SP!, {R0, R1, R2, R3}
  BL print_int_c_wrapper
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function print_int_c_wrapper
  @ calling function print_int_c_wrapper
  @ preparing arg _var273
  @ calling function car
  @ preparing arg _var272
  @ calling function cons
  @ preparing arg _var270
  @ int 5 with name _var270
  MOV R5, #5
  @ tagging int, reg R5
  LSL R5, R5, #3
  ORR R5, R5, #2
  @ preparing arg _var271
  @ int 6 with name _var271
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
  @ loading var "_var272"
  STMFD SP!, {R0}
  LDR R0, [FP, #-96]
  STMFD SP!, {R0, R1, R2, R3}
  BL car
  MOV R4, R0
  LDMFD SP!, {R0, R1, R2, R3}
  @ call end function car
  STMFD SP!, {R4}
  @ loading to reg arg 1
  @ loading var "_var273"
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
  .type dtcm_stack_size, %function
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
  @ preparing arg _var277
  @ int 1 with name _var276
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
  .type power6, %function
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
  @ int 1 with name _var279
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
  @ int 1 with name _var282
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var281"
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
  @ preparing arg _var285
  @ int 1 with name _var284
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
  @ loading var "_var281"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var286"
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
  .type power, %function
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
  @ int 1 with name _var287
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
  @ int 1 with name _var290
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var289"
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
  @ preparing arg _var293
  @ int 1 with name _var292
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
  @ loading var "_var289"
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
  .type poweri, %function
poweri:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @ int 5 with name _var296
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
  .type power2, %function
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
  @ int 1 with name _var297
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
  @ int 1 with name _var300
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var299"
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
  @ preparing arg _var302
  @ int 1 with name _var301
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
  @ loading var "_var299"
  STMFD SP!, {R3}
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var304"
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
  .type scheme_entry2, %function
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
  @ int 2 with name _var305
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ int 1 with name _var306
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
  @ int 1 with name _var309
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var308"
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
  @ preparing arg _var310
  @ int 3 with name _var310
  MOV R0, #3
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var311
  @ int 2 with name _var311
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
  @ int 2 with name _var314
  MOV R0, #2
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var313"
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
  @ preparing arg _var315
  @ bool False with name _var315
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
  @ loading var "_var313"
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
  @ loading var "_var308"
  LDR R3, [FP, #-16]
  @ else reg move
  @ loading var "_var313"
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
  .type print, %function
print:
  STMFD SP!, {LR}
  STMFD SP!, {R4, R5, R6, R7, R8, R9}
  STMFD SP!, {SL}
  STMFD SP!, {FP}
  MOV FP, SP
  @ body start
  @  -- if-then-else -- 
  @ calculating predicate
  @ bool False with name _var317
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
  @ int 10 with name _var319
  MOV R0, #10
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var318"
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
  @ bool True with name _var320
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
  @ bool False with name _var322
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
  @ int 11 with name _var324
  MOV R0, #11
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var323"
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
  @ bool True with name _var325
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
  @ preparing arg _var327
  @ int 32 with name _var327
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
  @ loading var "_var326"
  STMFD SP!, {R1}
  LDR R1, [FP, #-36]
  @ then reg move
  @ loading var "_var328"
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
  @ bool True with name _var329
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
  @ preparing arg _var331
  @ int 1 with name _var331
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
  @ loading var "_var330"
  STMFD SP!, {R1}
  LDR R1, [FP, #-44]
  @ then reg move
  @ loading var "_var332"
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
  @ bool True with name _var333
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
  @ preparing arg _var335
  @ int 1 with name _var335
  MOV R0, #1
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ preparing arg _var336
  @ int 2 with name _var336
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
  @ loading var "_var334"
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
  @ bool True with name _var338
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
  @ preparing arg _var340
  @ bool True with name _var340
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
  @ loading var "_var339"
  STMFD SP!, {R1}
  LDR R1, [FP, #-60]
  @ then reg move
  @ loading var "_var341"
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
  @ bool True with name _var342
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
  @ bool False with name _var344
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
  @ int 13 with name _var346
  MOV R0, #13
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ loading var "_var345"
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
  @ bool True with name _var347
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
  @ int 100 with name _var349
  MOV R0, #100
  @ tagging int, reg R0
  LSL R0, R0, #3
  ORR R0, R0, #2
  @ int 30 with name _var350
  MOV R1, #30
  @ tagging int, reg R1
  LSL R1, R1, #3
  ORR R1, R1, #2
  @ int 30 with name _var351
  MOV R2, #30
  @ tagging int, reg R2
  LSL R2, R2, #3
  ORR R2, R2, #2
  @ int 30 with name _var352
  MOV R3, #30
  @ tagging int, reg R3
  LSL R3, R3, #3
  ORR R3, R3, #2
  @ int 30 with name _var353
  MOV R4, #30
  @ tagging int, reg R4
  LSL R4, R4, #3
  ORR R4, R4, #2
  @ int 30 with name _var354
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
  @ loading var "_var348"
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
  @ loading var "_var348"
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
  @ loading var "_var345"
  LDR R1, [FP, #-76]
  @ else reg move
  @ loading var "_var348"
  LDR R0, [FP, #-84]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-84]
  STR R1, [FP, #-76]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-76
.Lend_53:
  @ loading var "_var343"
  LDR R1, [FP, #-68]
  @ then reg move
  @ loading var "_var345"
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
  @ loading var "_var343"
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
  @ loading var "_var339"
  LDR R1, [FP, #-60]
  @ else reg move
  @ loading var "_var343"
  LDR R0, [FP, #-68]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-68]
  STR R1, [FP, #-60]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-60
.Lend_47:
  @ loading var "_var334"
  LDR R1, [FP, #-52]
  @ else reg move
  @ loading var "_var339"
  LDR R0, [FP, #-60]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-60]
  STR R1, [FP, #-52]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-52
.Lend_44:
  @ loading var "_var330"
  LDR R1, [FP, #-44]
  @ else reg move
  @ loading var "_var334"
  LDR R0, [FP, #-52]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-52]
  STR R1, [FP, #-44]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-44
.Lend_41:
  @ loading var "_var326"
  LDR R1, [FP, #-36]
  @ else reg move
  @ loading var "_var330"
  LDR R0, [FP, #-44]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-44]
  STR R1, [FP, #-36]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-36
.Lend_38:
  @ loading var "_var323"
  LDR R1, [FP, #-28]
  @ else reg move
  @ loading var "_var326"
  LDR R0, [FP, #-36]
  MOV R1, R0
  @ else dump regs
  STR R0, [FP, #-36]
  STR R1, [FP, #-28]
  @ else reset SP
  MOV SP, FP
  SUB SP, SP, #-28
.Lend_35:
  @ loading var "_var321"
  LDR R1, [FP, #-20]
  @ then reg move
  @ loading var "_var323"
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
  @ loading var "_var321"
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
  @ loading var "_var318"
  LDR R2, [FP, #-12]
  @ else reg move
  @ loading var "_var321"
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
  .type eqQM, %function
eqQM:
  @ def:  eq? a b
  @ equality, compares addresses
  @ implemented incorrectly
   CMP R0, R1
   MOVEQ R0, #12
   MOVNE R0, #4
   BX LR
  
  @ Atom types predicates
  .type atomQM, %function
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
  
  .type numberQM, %function
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
  
  .type booleanQM, %function
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
  .type referenceQM, %function
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
  .type print_int_c_wrapper, %function
print_int_c_wrapper:
  @ def:  print_int_c_wrapper
  @ body
   STMFD SP!, {LR}
  @ untagging int, reg R0
   LSR R0, R0, #3
   BL print_int
   LDMFD SP!, {LR}
   BX LR
  
  .type print_bool_c_wrapper, %function
print_bool_c_wrapper:
  @ def:  print_bool_c_wrapper
  @ body
   STMFD SP!, {LR}
  @ untagging bool, reg R0
   LSR R0, R0, #3
   BL print_bool
   LDMFD SP!, {LR}
   BX LR
  

