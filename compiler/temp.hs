module Compiler where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Maybe
import Debug.Trace


import qualified ArmInstructions as Arm
import SchemeDataTypes

--------------------------------------------------------------------------------
-- Data types used by the compiler

type ArmCode = [Arm.ArmInstruction]

-- StackPos is calculated in relation to FramePointer
type StackPos = Int

-- Location of Variable
data VarLoc = LocNone | LocReg Arm.Register | LocStack StackPos | LocRegAndStack Arm.Register StackPos
              deriving (Eq, Show)

type Name = String

-- Can register be used
-- necessary for intermediate calculation
type Reserved = Bool

-- Current state of registers
type RegState =  Map.Map Arm.Register (Maybe Name)
-- Location of variables
type VarState = Map.Map Name VarLoc

-- Current depth of the stack, Full Descending Stack
type StackPtr = Int

-- Additional data necessary during compilation
type Something = Int

-- Current execution state of the compiled program
type ExecutionState = ( RegState, VarState, StackPtr, Something )

-- Keeps track of the current state
type Env r = State ExecutionState r

reportERROR = Arm.Comment "!!!!!!!!! - ERROR - !!!!!!!!!"

updateReg = True
updateRegNot = False
--------------------------------------------------------------------------------
-- State operations

resetEnv :: Env ()
resetEnv = do
  put (Map.fromList [], Map.fromList [], 0, -1)
  addRegisters

-- moves variable in register to stack
moveRegToStack :: Arm.Register -> Env (Name, ArmCode)
moveRegToStack reg = do
  (regState, varState, stackPtr, lastVarNo) <- get
  case Map.lookup reg regState of
    Just var ->
      case var of
        Nothing ->
          -- empty register
          return ("", [ reportERROR ])
        Just varName -> do
          -- var needs to be moved to the stack
          let newStackPtr = stackPtr - 4
          let newVarState = Map.insert varName (LocStack newStackPtr) varState
          let newRegState = Map.insert reg Nothing regState
          put (newRegState, newVarState, newStackPtr, lastVarNo)
          return (varName, [ Arm.STM Arm.FD Arm.SP updateReg [reg] ])

-- moves variable to register if it is on the stack
-- desired register can be provided
loadVarToRegister :: Name -> Maybe Arm.Register -> Env (Arm.Register, ArmCode)
loadVarToRegister ident desReg = do
  (regState, varState, stackPtr, lastVarNo) <- get
  case Map.lookup ident varState of
    Nothing -> return (Arm.NoReg, [reportERROR])
    Just (LocReg reg) ->
      -- already in register
      return (reg, [])
    Just (LocStack stackPos) -> do
      -- get register
      (reg, getRegC) <- case desReg of
                          Nothing -> getFreeRegister
                          Just reg -> do
                            (mvVar, mvCode) <- moveRegToStack reg
                            return (reg, mvCode)
      -- reg is free now, move there var
      let moveToReg = [ Arm.LDR reg Arm.FP (Arm.O2ShImm stackPos) Arm.Offset ]
      -- update position of var
      (regState, varState, stackPtr) <- get
      let newVarState = Map.insert ident (LocReg reg) varState
      let newRegState = Map.insert reg (Just ident) regState
      put (newRegState, newVarState, stackPtr, lastVarNo)
      -- return
      return (reg,
              [ Arm.Comment $ "loading var " ++ (show ident)] ++
              getRegC ++
              moveToReg)

-- get register
getFreeRegister :: Env (Arm.Register, ArmCode)
getFreeRegister = do
  (regState, varState, stackPtr, lastVarNo) <- get
  let freeReg = Map.foldrWithKey (\k ident acc -> if not (id == Nothing) then acc else k:acc) [] regState
  if not (length freeReg == 0)
    then do
      -- at least one free register, not used
      -- take first
      let reg = freeReg !! 0
      return (reg, [])
    else do
      -- no free registers
      -- needs to get register
      -- it might be used but can not be reserved
      let regs = Map.foldrWithKey (\k ident acc -> k:acc) [] regState
      if length regs == 0
        then return (Arm.NoReg, [ reportERROR ])
        else do
          let reg = regs !! 0
          -- move current var in reg to the stack
          (varName, moveToStack) <- moveRegToStack reg
          return (reg,
                  [ Arm.Comment $ "moving to stack reg " ++ (show varName)] ++
                  moveToStack)

-- get new free temporary register and reserve that register
getRegWithNewVar :: Env (Name, Arm.Register, ArmCode)
getRegWithNewVar = do
  (reg, code) <- getFreeRegister
  (regState, varState, stackPtr, lastVarNo) <- get
  -- generate new var name
  let newVarNo = lastVarNo + 1
  let varName = "_var" + (show newVarNo)
  -- add info to state
  let newVarState = Map.insert varName (LocReg reg) varState
  let newRegState = Map.insert reg (Just varName) regState
  put (newRegState, newVarState, stackPtr, newVarNo)
  return (varName, reg, code)

-- adding fun args
addFunArgsToEnv :: [Name] -> Env ()
addFunArgsToEnv args = do
  -- add arguments 0-3 to registers
  addArgToReg args Arm.R0 0
  addArgToReg args Arm.R1 1
  addArgToReg args Arm.R2 2
  addArgToReg args Arm.R3 3
  -- add args from 4-...
  addArgToStack args 4
  where
    addArgToReg args reg no =
      if no > length args - 1
        then return ()
        else do
          let arg = args !! no
          (regState, varState, stackPtr, lastVarNo) <- get
          let newVarState = Map.insert arg (LocReg reg) varState
          let newRegState = Map.insert reg (Just arg) regState
          put (newRegState, newVarState, stackPtr, lastVarNo)
    addArgToStack args no =
      if no > length args - 1
        then return ()
        else do
          -- calculate pos: arg_n-arg_4, LR, R4-R9, SL and no
          let pos =  (4 + 6*4 + 4 ) + (no - 3)*4
          let arg = args !! no
          (regState, varState, stackPtr, lastVarNo) <- get
          let newVarState = Map.insert arg (LocStack pos) varState
          put (regState, newVarState, stackPtr, lastVarNo)
          -- add rest args
          addArgToStack args (no + 1)

removeFunArgsFromEnv :: [Name] -> Env ()
removeFunArgsFromEnv args = do
  removeArg args Arm.R0 0
  removeArg args Arm.R1 1
  removeArg args Arm.R2 2
  removeArg args Arm.R3 3
  where
    removeArg args reg no =
      if no > length args - 1
        then return ()
        else do
          let arg = args !! no
          (regState, varState, stackPtr, lastVarNo) <- get
          let newVarState = Map.delete arg varState
          let newRegState = Map.insert reg (Nothing) regState
          put (newRegState, newVarState, stackPtr, lastVarNo)

--------------------------------------------------------------------------------
-- Tagging operations

-- general untagging
tagReg :: Arm.Register -> SchemeInst -> ArmCode

tagReg reg (Number _)  =
  [ Arm.Comment $ "tagging number, reg " ++ (show reg),
    Arm.LSL reg reg (Arm.O2NoShImm 2),
    Arm.ORR reg reg (Arm.O2ShImm 2) ]

tagReg reg _ =
  [ Arm.Comment $ "tagging number, reg " ++ (show reg),
    Arm.LSL reg reg (Arm.O2NoShImm 2),
    Arm.ORR reg reg (Arm.O2ShImm 2) ]

-- general untagging
untagReg :: Arm.Register -> SchemeInst -> ArmCode

untagReg reg (Number _) =
  [ Arm.Comment $ "untagging number, reg " ++ (show reg),
    Arm.LSR reg reg (Arm.O2NoShImm 2) ]

untagReg reg _ =
  [ Arm.Comment $ "untagging number, reg " ++ (show reg),
    Arm.LSR reg reg (Arm.O2NoShImm 2) ]
  
  
--------------------------------------------------------------------------------
-- calling functions

callFun ident args = do
  -- compile all arguments
  (arg0To3CompNames, arg0To3CompC) <- compArg0To3 args
  (argFrom4CompNames, argFrom4CompC) <- compArgStackFrom4 args
  -- free r4, it will be used as empty register to dump args from 04-
  (_, mvR4ToStack) <- moveRegToStack Arm.R4
  -- load compiled values of arguments 0-3 to registers R0 - R3
  arg0To3LdC <- loadArgsToRegs arg0To3CompNames [Arm.R0, Arm.R1, Arm.R2, Arm.R3]
  -- save to stack compiled values of arguments from 4 using empty register R4
  argFrom4LdC <- loadArgsToStack argFrom4CompNames Arm.R4
  -- dump current state of scratch registers: r0-r3 and r12
  regsToDumpRest <- inUseRegs [ Arm.R0, Arm.R1, Arm.R2, Arm.R3, Arm.R12 ]
  let scratchRegDumpC = if length regsToDumpRest == 0
                          then []
                          else [ Arm.STM Arm.FD Arm.SP updateReg regsToDumpRest ]
  -- call fun, always result is in R0
  let callFunC = [ Arm.BL Arm.CondNO ident ]
  -- result in R0 move to other register
  (retName, retReg, retCode) <- getRegWithNewVar
  let mvValR0ToRet = [ Arm.MOV retReg (Arm.O2ShReg Arm.R0 Arm.NoShift) ]
  -- reset SP
  let restStackC = if length args > 4
                     then [ Arm.Sub Arm.SP Arm.SP (Arm.O2ShImm (4 * (length args - 4))) ]
                     else []
  -- load R0, R1, R2, R3 and R12
  let restRegC = if length regsToDumpRest == 0
                    then []
                    else [ Arm.LDM Arm.FD Arm.SP updateReg regsToDumpRest ]
  -- result in retReg
  return (retName, retReg,
          [ Arm.Comment $ "calling function " ++ ident ] ++
          arg03SetRegC ++
          scratchRegDumpC ++
          arg04SetRegC ++
          callFunC ++
          retTempCode ++
          mvValR0ToOther ++
          restStackC ++
          restRegC ++
          [ Arm.Comment $ "call end function " ++ ident ])
  where
    compArg0To3 args = do
      (arg0Name, arg0C) <- compArg args 0 Arm.R0
      (arg1Name, arg1C) <- compArg args 1 Arm.R1
      (arg2Name, arg2C) <- compArg args 2 Arm.R2
      (arg3Name, arg3C) <- compArg args 3 Arm.R3
      return (arg0Name ++ arg1Name ++ arg2Name ++ arg3Name,
              arg0C ++ arg1C ++ arg2C ++ arg3C)

      where 
        compArg args no reg =
          if no > length args - 1
            then return ([], [])
            else do
              (compName, compReg, compCode) <- compile $ args !! no
              return ([ compName ],
                      [ Arm.Comment $ "preparing arg " ++ (show no) ++ " for reg " ++ (show reg) ] ++
                      compCode)
    compArgStackFrom4 args =
      setArg args 4
      where
        setArg args no =
          if no > length args - 1
            then return ([], [])
            else do
              (compName, compReg, compCode) <- compile $ args !! no
              -- push previous argument
              (restNames, restCode) <- setArg args (no + 1)
              return (compName ++ restNames,
                      [ Arm.Comment $ "preparing arg " ++ (show no) ] ++
                       compCode ++
                       [ Arm.STM Arm.FD Arm.SP updateReg [compReg] ] ++
                       restCode)
    loadArgsToRegs [] [] = return []
    loadArgsToRegs [vn:vns] [reg:regs] = do
      code <- loadVarToRegister vn (Just reg)
      restC <- loadArgsToRegs vns regs
      return $ [ Arm.Comment $ "loading to reg arg " ++ (show $ 4 - length (vn ++ vns)) ] ++
               code ++ restC
    loadArgsToStack [] emReg = return []
    loadArgsToStack [vn:vns] emReg = do
      (regState, varState, stackPtr, lastVarNo) <- get
      let code = case Map.fromJust ( Map.lookup ident varState ) of
                  LocReg reg -> [ Arm.STM Arm.FD Arm.SP updateReg [reg] ]
                  LocStack pos -> [ Arm.LDR emReg Arm.FP (Arm.O2ShImm pos) Arm.Offset ] ++
                                  [ Arm.STM Arm.FD Arm.SP updateReg [emReg] ]
      rest <- loadArgsToStack vns emReg
      -- reverse order of arugments
      return $ rest ++
               [ Arm.Comment $ "loading to stack arg " ++ (show $ 4 - length (vn ++ vns)) ] ++ 
               code

defineFun ident args body = do
  -- reset env
  resetEnv
  -- add definitions of function's arguments to environment
  addFunArgsToEnv args
  -- label for function
  let funLabel = [ Arm.Label ident ]
  -- save LR on the stack
  let lrDumpC = [ Arm.STM Arm.FD Arm.SP updateReg [Arm.LR] ]
  -- dump non-scratch registers: r4-r9
  let nonscratchRegDumpC = [ Arm.STM Arm.FD Arm.SP updateReg [Arm.R4, Arm.R5, Arm.R6, Arm.R7, Arm.R8, Arm.R9] ]
  -- save SL on the stack
  let slDumpC = [ Arm.STM Arm.FD Arm.SP updateReg [Arm.SL] ]
  -- save FP on the stack
  let fpDumpC = [ Arm.STM Arm.FD Arm.SP updateReg [Arm.FP] ]
  -- set FP to current SP
  let spToFPC = [ Arm.MOV Arm.FP (Arm.O2ShReg Arm.SP Arm.NoShift) ]
  -- body
  (bodyReg, bodyC) <- compBody body
  -- move returned value to R0
  let mvValToR0 = [ Arm.MOV Arm.R0 (Arm.O2ShReg bodyReg Arm.NoShift) ]
  -- set SP to current FP
  let fpToSPC = [ Arm.MOV Arm.SP (Arm.O2ShReg Arm.FP Arm.NoShift) ]
  -- restore FP from the stack
  let fpRestC = [ Arm.LDM Arm.FD Arm.SP updateReg [Arm.FP] ]
  -- restore SL from the stack
  let slRestC = [ Arm.LDM Arm.FD Arm.SP updateReg [Arm.SL] ]
  -- restore non-scratch registers: r4-r9
  let nonscratchRegRestC = [ Arm.LDM Arm.FD Arm.SP updateReg [Arm.R4, Arm.R5, Arm.R6, Arm.R7, Arm.R8, Arm.R9] ]
  -- restore LR from the stack
  let lrRestC = [ Arm.LDM Arm.FD Arm.SP updateReg [Arm.LR] ]
  -- return to caller
  let retC = [ Arm.BX Arm.CondNO Arm.LR ]
  -- remove definitions of function's arguments to environment
  removeFunArgsFromEnv args

  return $ funLabel ++
           lrDumpC ++
           nonscratchRegDumpC ++
           slDumpC ++
           fpDumpC ++
           spToFPC ++
           [ Arm.Comment "body start" ] ++
           bodyC ++
           mvValToR0 ++
           [ Arm.Comment "body end" ] ++
           fpToSPC ++
           fpRestC ++
           slRestC ++
           nonscratchRegRestC ++
           lrRestC ++
           retC

  where
    compBody body = do
      (compReg, compCode) <- compile body
      return $ (compReg, compCode)

  
{-
Caller
  * Save r0-r3 and r12 on the stack
  * Set arguments 0-3 in register r0-r3 and the rest arguments on the stack
  *  -- CALL --
  * Subtract from value of the SP number of arguments
  * Restore registers r0-r3 and r12 from the stack


Calling
  * Save LR on the stack
  * Save non scratch registers r4-r9
  * Save SL register on the stack
  * Save FP on the stack
  * Set new FP to current value of SP
  * -- DO --
  * Load value of FP to SP
  * Restore SL register
  * Restore r4-r9 registers
  * Restore LR register
  * Return to the caller
  -}
--------------------------------------------------------------------------------
-- Compiling code

-- functions compiling Scheme code
-- it returns copiled code and register containing returned value of the code
compile :: SchemeInst -> Env (Name, Arm.Register, ArmCode)

-- Comment
compile (List (Atom "Comment" : String cm : [])) = return ("", Arm.NoReg, [Arm.Comment cm])

-- Inline
compile (List (Atom "Inline" : String inl : [])) = return ("", Arm.NoReg, [Arm.Inline inl])

-- Function
compile (List (Atom "define" : List (Atom name : args) : body : [])) = do
  let argNames = unArgs args
  funCode <- defineFun name argNames body
  return ("", Arm.NoReg, funCode)

  where
    unArgs [] = []
    unArgs (Atom atom : args) = [ atom ] ++ unArgs args

-- Constants
compile (Number num) = do
  (name, reg, code) <- getRegWithNewVar
  return $ (name, reg, 
            [ Arm.Comment $ "number " ++ (show num) ] ++
            code ++
            [ Arm.MOV reg (Arm.O2ShImm num) ] ++
            tagReg reg (Number num))

-- Variable
compile (Atom varName) = do
  (reg, code) <- loadVarToRegister varName
  return (varName, reg, code)

-- basic binary operations
compile (List (Atom "+" : arg1 : arg2 : [])) = do
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Add retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  
compile (List (Atom "-" : arg1 : arg2 : [])) =
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Sub retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  
compile (List (Atom "*" : arg1 : arg2 : [])) =
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Mul retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  
  

-- If it was not recognized it must be a function
compile (List (Atom name : args)) = do
  callFun name args

-- Basic binary operations
compileBinOp :: (Arm.Register -> Arm.Register -> Arm.Register -> ArmCode) -> 
                 SchemeInst -> SchemeInst -> Env (Name, Arm.Register, ArmCode)
compileBinOp binOp arg1 arg2 = do
  -- compile arugments
  (arg1Name, arg1Reg, arg1C) <- compile arg1
  (arg2Name, arg2Reg, arg2C) <- compile arg2

  -- untag
  let arg1UntagC = untagReg arg1Reg arg1
  -- if arguments are the same don't untag two times
  -- the same register
  let arg2UntagC = if arg1Reg == arg2Reg
                    then []
                    else untagReg arg2Reg arg2

  (retName, retReg, retRegC) <- getRegWithNewVar
  (arg1LdReg, arg1LdC) <- loadVarToRegister arg1Name Nothing
  (arg2LdReg, arg2LdC) <- loadVarToRegister arg2Name Nothing
  let addC = binOp retReg arg1LdReg arg2LdReg

  -- tag result
  let addResTagC = tagReg retReg arg1
  return (retName, retReg,
          arg1C ++
          arg2C ++
          arg1UntagC ++
          arg2UntagC ++
          retRegC ++
          arg1LdC ++
          arg2LdC ++
          addC ++
          addResTagC)


--------------------------------------------------------------------------------
-- Starting compiler

addRegisters :: Env ()
addRegisters = do
  let allRegs = Map.fromList [free Arm.R0, free Arm.R1, free Arm.R2, free Arm.R3,
                              free Arm.R4, free Arm.R5, free Arm.R6, free Arm.R7,
                              free Arm.R8, free Arm.R9, free Arm.R10, free Arm.R11,
                              free Arm.R12, free Arm.R13, free Arm.R14, free Arm.R15]
  (regState, varState, stackPtr) <- get
  let newRegState = Map.union regState allRegs -- already specified reg state is mort important
  put (newRegState, varState, stackPtr)

  where
    free reg = (reg, (False, Nothing))

programHeader = [ Arm.Special ".global scheme_entry" ]
  
compileCode :: [SchemeInst] -> Env [Arm.ArmInstruction]
compileCode body = do
  addRegisters
  --reserveRegisters
  compCode <- compBody body
  return $ programHeader ++
           compCode

  where
    compBody [] = return []
    compBody (fun:rest) = do
      (funReg, funCode) <- compile fun
      restCode <- compBody rest
      return $ funCode ++ restCode


--------------------------------------------------------------------------------
-- TO DO

-- if, else
-- built in function, list? etc
-- multi argument functions
-- better tagging


--------------------------------------------------------------------------------
-- Errors and limitations

-- deep nesting won't compile correctly
-- (* 2 (* 2 (* 2 (+ 1 (+ 3)))))

