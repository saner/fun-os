(include "common.scm")

(load "lib.scm")
(include "os.scm")

(process (attt a)
         (+ 1 2))

(process (att2 a)
         (+ 1 2))

(assembler (sample-process no)
  (comment "prologue start")
  (STMFD SP!, {R0})
  (MOV FP, SP)
  (comment "prologue end")


  (comment "process body: start")
  (comment "process body: end")

  (comment "epilog start")
  (comment "restore process no")
  (MOV SP, FP)
  (LDMFD SP!, {R9})
  (comment "epilog end")

  (comment "disable interrupts")
  (LDR R5, REG_IME)
  (MOV R6, #0)
  (LDR R7, [R5])
  (STR R6, [R5])

  (comment "remove process")
  (comment "it should be enough")
  (comment "scheduler won't select this process")
  (comment "because it is removed (no PCB)")
  (MOV R0, R9)
  (BL remove_process)
  (comment "this process is running, set active process to -1")
  (comment "next time scheduler runs, it will select another process")
  (MOV R8, #-1)
  (LDR R8, [SL, #8])

  (comment "restore interrupts")
  (LDR R5, REG_IME)
  (STR R7, [R5])


  (comment "enter infinite loop")
  (comment "until scheduler removes this process")
  (sample_inf_loop:)
  (B sample_inf_loop)

  (comment "process end"))

(assembler (idle-process-2)

  (MOV R0, #0)
  (MOV R1, #11)
  (MOV R2, #21)
  (MOV R3, #31)
  (MOV R4, #41)
  (MOV R5, #51)
  (MOV R6, #61)
  (MOV R7, #71)
  (MOV R8, #81)
  (MOV R9, #91)
  (MOV R12, #121)

  (idle_2_inf_loop:)
  (B idle_2_inf_loop)

  (comment "process end"))


(global-fun user-code)
(define (user-code)
  (begin 
    (enable-process (add-process first-proc))
    (enable-process (add-process second-proc))))

(process (first-proc)
         (print-int 101))

(process (second-proc)
         (print-int 102))
