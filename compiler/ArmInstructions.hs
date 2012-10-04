module ArmInstructions where

import Data.List

-- Label
type Label = String

-- Condition
data Condition = CondNO -- No conditon, NO ARM instruction
               | CondEQ -- Equal
               | CondNE -- Not equal
               | CondLT -- Signed less than
               | CondGT -- Signed greater than
               | CondLE -- Signed <=
               | CondGE -- Signed >=
               deriving Eq

instance Show Condition where
  show CondNO = ""
  show CondEQ = "EQ"
  show CondNE = "NE"
  show CondLT = "LT"
  show CondGT = "GT"
  show CondLE = "LE"
  show CondGE = "GE"

-- Registers
---- Static Link R10
---- Frame Pointer R11
---- Inter Process R12
---- Stack Pointer R13
---- Location Register R14
---- Program Counter R15
data Register = R0  | R1  | R2  | R3  | R4  | R5  | R6  | R7  
              | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
              | SL  | FP  | IP  | SP  | LR  | PC | NoReg
              deriving (Show, Eq, Ord)

data Shift = ShLSL | ShLSR | ShASR | ShROR
             deriving (Show, Eq)

data OpSh = NoShift
        | Shift Shift Integer
        deriving Eq

-- Operand2Sh
data Operand2Sh = O2ShReg Register OpSh
                | O2ShImm Int
                deriving Eq
data Operand2NoSh = O2NoShReg Register
                  | O2NoShImm Int
                  deriving Eq

instance Show Operand2Sh where
  show (O2ShReg reg NoShift) = show reg
  show (O2ShReg reg (Shift s x)) = (show reg) ++ ", " ++ (show s) ++ " #" ++ (show x)
  show (O2ShImm x) = "#" ++ (show x)

instance Show Operand2NoSh where
  show (O2NoShReg reg) = show reg
  show (O2NoShImm x) = "#" ++ (show x)

type UpdateRegister = Bool

data LdrStrAddr = PreIndex UpdateRegister -- Rd, [Rn, Rm] or Rd, [Rn, Rm]!
                | PostIndex -- Rd, [Rn], Rm
                | Offset -- Rd, [Rn, Rm]

data DirMet = IA -- Increase After
            | IB -- Increase Before
            | DA -- Decrease After
            | DB -- Decrease Before
            | FD -- Full Descending
            | FA -- Full Ascending
            | ED -- Full Descending
            | EA -- Full Ascending
            deriving (Show, Eq)

-- All used ARM instructions
data ArmInstruction = Add Register Register Operand2Sh
                    | Sub Register Register Operand2Sh
                    | Mul Register Register Operand2Sh
                    -- Branch
                    | B Condition Label -- with link
                    | BL Condition Label -- and exchange
                    | BX Condition Register -- with link and exchange
                    -- Compare
                    | CMP Register Operand2Sh -- Update CPSR flags on Rn - Op2
                    | CMN Register Operand2Sh -- Update CPSR flags on Rn + Op2
                    -- Logical Shift
                    | LSL Register Register Operand2NoSh
                    | LSR Register Register Operand2NoSh
                    -- Logical
                    | TST Register Operand2Sh -- Update CPSR flags on Rn AND Op2
                    | TEQ Register Operand2Sh -- Update CPSR flags on Rn EOR Op2
                    | AND Register Register Operand2Sh -- Rd := Rn AND Op2
                    | EOR Register Register Operand2Sh -- Rd := Rn EOR Op2
                    | ORR Register Register Operand2Sh -- Rd := Rn OR Op2
                    | ORN Register Register Operand2Sh -- Rd := Rn OR NOT Op2
                    -- Move
                    | MOV Register Operand2Sh -- Rd := Op2
                    -- Load & Store
                    | LDR Register Register Operand2Sh LdrStrAddr
                    | STR Register Register Operand2Sh LdrStrAddr
                    -- Load & Store multiple
                    | LDM DirMet Register UpdateRegister [Register]
                    | STM DirMet Register UpdateRegister [Register]
                    -- Label
                    | Label Label
                    -- Special
                    | Special String
                    -- Inline
                    | Inline String
                    -- Comment
                    | Comment String


armInstCnd ins cnd lbl =  ins ++ (show cnd) ++ " " ++ lbl
armInst2 ins reg1 reg2 =  ins ++ " " ++ (show reg1) ++ ", " ++ (show reg2)
armInst2Str ins reg1 reg2Str =  ins ++ " " ++ (show reg1) ++ ", " ++ reg2Str
armInst3 ins reg1 reg2 reg3 =  ins ++ " " ++ (show reg1) ++ ", " ++ (show reg2) ++ ", " ++ (show reg3)

op2StrLdrAddr reg2 op2 (PreIndex False) = "[" ++ (show reg2) ++ ", " ++ (show op2) ++ "]"
op2StrLdrAddr reg2 op2 (PreIndex True) = "[" ++ (show reg2) ++ ", " ++ (show op2) ++ "]!"
op2StrLdrAddr reg2 op2 PostIndex = "[" ++ (show reg2) ++ "], " ++ (show op2)
op2StrLdrAddr reg2 op2 Offset = "[" ++ (show reg2) ++ ", " ++ (show op2) ++ "]"

ldmStr :: String -> DirMet -> Register -> UpdateRegister -> [Register] -> String
ldmStr inst dirMet reg1 upd1 regs =
  inst ++ (show dirMet) ++ " " ++ (show reg1) ++ (updStr upd1) ++ ", {" ++ (regsStr regs) ++ "}"
  where
    regsStr [] = ""
    regsStr (h:[]) = show h
    regsStr (h:t) = (show h) ++ ", " ++ (regsStr t)
    updStr True = "!"
    updStr False = "!"

instance Show ArmInstruction where
  show (Add reg1 reg2 reg3) = armInst3 "ADD" reg1 reg2 reg3
  show (Sub reg1 reg2 reg3) = armInst3 "SUB" reg1 reg2 reg3
  show (Mul reg1 reg2 reg3) = armInst3 "MUL" reg1 reg2 reg3
  show (B cnd lbl) = armInstCnd "B" cnd lbl 
  show (BL cnd lbl) = armInstCnd "BL" cnd lbl 
  show (BX cnd reg) = armInstCnd "BX" cnd (show reg) 
  show (CMP reg op2) = armInst2 "CMP" reg op2
  show (CMN reg op2) = armInst2 "CMN" reg op2
  show (TST reg op2) = armInst2 "TST" reg op2
  show (TEQ reg op2) = armInst2 "TEQ" reg op2
  show (AND reg1 reg2 op2) = armInst3 "AND" reg1 reg2 op2
  show (EOR reg1 reg2 op2) = armInst3 "EOR" reg1 reg2 op2
  show (ORR reg1 reg2 op2) = armInst3 "ORR" reg1 reg2 op2
  show (ORN reg1 reg2 op2) = armInst3 "ORN" reg1 reg2 op2
  -- Logical Shift
  show (LSL reg1 reg2 reg3) = armInst3 "LSL" reg1 reg2 reg3
  show (LSR reg1 reg2 reg3) = armInst3 "LSR" reg1 reg2 reg3
  -- Move
  show (MOV reg op2) = armInst2 "MOV" reg op2
  -- Load
  show (LDR reg1 reg2 op2 addr) =
    let reg2op2addr = op2StrLdrAddr reg2 op2 addr
    in armInst2Str "LDR" reg1 reg2op2addr
  -- Store
  show (STR reg1 reg2 op2 addr) =
    let reg2op2addr = op2StrLdrAddr reg2 op2 addr
    in armInst2Str "STR" reg1 reg2op2addr
  show (LDM dirMet reg1 upd1 regs) = 
    ldmStr "LDM" dirMet reg1 upd1 regs
  show (STM dirMet reg1 upd1 regs) = 
    ldmStr "STM" dirMet reg1 upd1 regs
  show (Label label) = label ++ ":"
  show (Special spec) = spec
  show (Inline inline) = inline
  show (Comment cm) = "@ " ++ cm
