module Compiler where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Debug.Trace


import qualified ArmInstructions as Arm
import SchemeDataTypes
import CompUtils

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
type Something = (Int, Int)

-- Current execution state of the compiled program
type ExecutionState = ( RegState, VarState, StackPtr, Something )

-- Keeps track of the current state
type Env r = State ExecutionState r

reportERROR desc = Arm.Comment $ "!!!!!!!!! - ERROR : " ++ desc

updateReg = True
updateRegNot = False
--------------------------------------------------------------------------------
-- State operations

resetEnv :: Env ()
resetEnv = do
  (envReg, envVar, envStackPtr, envCountPtr) <- get
  put (Map.fromList [], Map.fromList [], 0, envCountPtr)
  addRegisters

-- moves variable in register to stack
moveRegToStack :: Arm.Register -> Env (Name, ArmCode)
moveRegToStack reg = do
  (regState, varState, stackPtr, some) <- get
  case Map.lookup reg regState of
    Just var ->
      case var of
        Nothing ->
          -- empty register
          return ("", [ reportERROR $ "moveRegToStack: no var in reg: " ++ (show reg) ])
        Just varName -> do
          case Map.lookup varName varState of
            Just (LocReg reg) -> do
              -- var needs to be moved to the stack
              let newStackPtr = stackPtr - 4
              let newVarState = Map.insert varName (LocStack newStackPtr) varState
              let newRegState = Map.insert reg Nothing regState
              put (newRegState, newVarState, newStackPtr, some)
              return (varName, [ Arm.STM Arm.FD Arm.SP updateReg [reg] ])
            Just (LocRegAndStack reg stackPos) -> do
              -- var needs to be moved to the stack
              -- to already reserved place
              let newVarState = Map.insert varName (LocStack stackPos) varState
              let newRegState = Map.insert reg Nothing regState
              put (newRegState, newVarState, stackPtr, some)
              return (varName, [ Arm.STR reg Arm.FP (Arm.O2ShImm stackPos) Arm.Offset ])

-- moves variable to register if it is on the stack
-- desired register can be provided, and restricted registers
loadVarToRegister :: Name -> Maybe Arm.Register -> [Arm.Register] -> Env (Arm.Register, ArmCode)
loadVarToRegister ident desReg restRegs = do
  (regState, varState, stackPtr, some) <- get
  case Map.lookup ident varState of
    Nothing -> return (Arm.NoReg, [reportERROR "loadVarToRegister: var was not found in state"])
    Just (LocReg reg) ->
      -- already in register
      case desReg of
        Nothing -> return (reg, [])
        Just desReg ->
          if desReg == reg
            then return (desReg, [])
            else do
              -- free reg
              isUsed <- inUseReg desReg
              (_, freeRegCode) <- if isUsed
                                    then moveRegToStack desReg
                                    else return ("", [])
              -- update position of var
              (regState, varState, stackPtr, some) <- get
              let newVarState = Map.insert ident (LocReg desReg) varState
              let newRegState1 = Map.insert reg Nothing regState
              let newRegState2 = Map.insert desReg (Just ident) newRegState1
              put (newRegState2, newVarState, stackPtr, some)
              return (desReg, freeRegCode ++ [ Arm.MOV desReg (Arm.O2ShReg reg Arm.NoShift) ])
    Just (LocRegAndStack reg stackPos) ->
      -- already in register
      case desReg of
        Nothing -> return (reg, [])
        Just desReg ->
          if desReg == reg
            then return (desReg, [])
            else do
              -- free reg
              isUsed <- inUseReg desReg
              (_, freeRegCode) <- if isUsed
                                    then moveRegToStack desReg
                                    else return ("", [])
              -- update position of var
              (regState, varState, stackPtr, some) <- get
              let newVarState = Map.insert ident (LocRegAndStack desReg stackPos) varState
              let newRegState1 = Map.insert reg Nothing regState
              let newRegState2 = Map.insert desReg (Just ident) newRegState1
              put (newRegState2, newVarState, stackPtr, some)
              return (desReg, freeRegCode ++ [ Arm.MOV desReg (Arm.O2ShReg reg Arm.NoShift) ])
    Just (LocStack stackPos) -> do
      -- get register
      (reg, getRegC) <- case desReg of
                          Nothing -> getFreeRegister restRegs
                          Just reg -> do
                            isUsed <- inUseReg reg
                            (mvVar, mvCode) <- if isUsed
                                                  then moveRegToStack reg
                                                  else return ("", [])
                            return (reg, mvCode)
      -- reg is free now, move there var
      let moveToReg = [ Arm.LDR reg Arm.FP (Arm.O2ShImm stackPos) Arm.Offset ]
      -- update position of var
      (regState, varState, stackPtr, some) <- get
      let newVarState = Map.insert ident (LocRegAndStack reg stackPos) varState
      let newRegState = Map.insert reg (Just ident) regState
      put (newRegState, newVarState, stackPtr, some)
      -- return
      return (reg,
              [ Arm.Comment $ "loading var " ++ (show ident)] ++
              getRegC ++
              moveToReg)

-- get register
getFreeRegister :: [Arm.Register] -> Env (Arm.Register, ArmCode)
getFreeRegister restRegs = do
  (regState, varState, stackPtr, some) <- get
  let freeReg = Map.foldrWithKey (\k ident acc -> if elem k restRegs || not (ident == Nothing) then acc else k:acc) [] regState
  if not (length freeReg == 0)
    then do
      -- at least one free register, not used
      -- take first
      let reg = freeReg !! 0
      return (reg, [])
    else do
      -- no free registers
      -- needs to get register
      -- it might be used but can not be restricted
      let regs = Map.foldrWithKey (\k ident acc -> if elem k restRegs then acc else k:acc) [] regState
      if length regs == 0
        then return (Arm.NoReg, [ reportERROR "getFreeRegister: there are no free regs" ])
        else do
          let reg = regs !! 0
          -- move current var in reg to the stack
          (varName, moveToStack) <- moveRegToStack reg
          return (reg,
                  [ Arm.Comment $ "moving to stack var " ++ (show varName) ++ " from reg " ++ (show reg)] ++
                  moveToStack)

-- get new free reg and create new var in it
getRegWithNewVar :: (Maybe Name) -> [Arm.Register] -> Env (Name, Arm.Register, ArmCode)
getRegWithNewVar vName restRegs = do
  (reg, code) <- getFreeRegister restRegs
  (regState, varState, stackPtr, (lastVarNo, lastLblNo)) <- get
  -- generate new var name
  let (varName, newVarNo) = case vName of
                              Nothing ->
                                    let no = lastVarNo + 1
                                    in ("_var" ++ (show no), no)
                              Just name -> (name, lastVarNo)
  -- add info to state
  let newVarState = Map.insert varName (LocReg reg) varState
  let newRegState = Map.insert reg (Just varName) regState
  put (newRegState, newVarState, stackPtr, (newVarNo, lastLblNo))
  return (varName, reg, code)

setVarToReg :: Name -> Arm.Register -> Env ()
setVarToReg var reg = do
  (regState, varState, stackPtr, some) <- get
  let newVarState = Map.insert var (LocReg reg) varState
  let newRegState = Map.insert reg (Just var) regState
  put (newRegState, newVarState, stackPtr, some)


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
          setVarToReg arg reg
    addArgToStack args no =
      if no > length args - 1
        then return ()
        else do
          -- calculate pos: arg_n-arg_4, LR, R4-R9, SL and no
          let pos =  (4 + 6*4 + 4 ) + (no - 3)*4
          let arg = args !! no
          (regState, varState, stackPtr, some) <- get
          let newVarState = Map.insert arg (LocStack pos) varState
          put (regState, newVarState, stackPtr, some)
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
          (regState, varState, stackPtr, some) <- get
          let newVarState = Map.delete arg varState
          let newRegState = Map.insert reg Nothing regState
          put (newRegState, newVarState, stackPtr, some)


--------------------------------------------------------------------------------
-- labels

createLabel :: String -> Env String
createLabel name = do
  (regState, varState, stackPtr, (lastVarNo, lastLblNo)) <- get
  let newLblNo = lastLblNo + 1
  put (regState, varState, stackPtr, (lastVarNo, newLblNo))
  return $ ".L" ++ name ++ "_" ++ (show newLblNo)

--------------------------------------------------------------------------------
-- Tagging operations

type TagType = SchemeType

-- general untagging
tagReg :: Arm.Register -> TagType -> ArmCode

tagReg reg TInt =
  [ Arm.Comment $ "tagging int, reg " ++ (show reg),
    Arm.LSL reg reg (Arm.O2NoShImm 3),
    Arm.ORR reg reg (Arm.O2ShImm 2) ]

tagReg reg TBool =
  [ Arm.Comment $ "tagging bool, reg " ++ (show reg),
    Arm.LSL reg reg (Arm.O2NoShImm 3),
    Arm.ORR reg reg (Arm.O2ShImm 4) ]

-- general untagging
untagReg :: Arm.Register -> TagType -> ArmCode

untagReg reg TInt =
  [ Arm.Comment $ "untagging int, reg " ++ (show reg),
    Arm.LSR reg reg (Arm.O2NoShImm 3)]
  
untagReg reg TBool =
  [ Arm.Comment $ "untagging bool, reg " ++ (show reg),
    Arm.LSR reg reg (Arm.O2NoShImm 3)]

trueTagged = 12
falseTagged = 4

--------------------------------------------------------------------------------
-- calling functions

inUseRegs :: [Arm.Register] -> Env [Arm.Register]
inUseRegs regs = do
  -- not all might be in use
  filterM (\reg -> inUseReg reg) regs

inUseReg :: Arm.Register -> Env Bool
inUseReg reg = do
  (regState, varState, stackPtr, some) <- get
  case (Map.lookup reg regState) of
    Just var ->
      case var of
        Just _ -> return True
        Nothing -> return False

callFun ident args = do
  -- compile all arguments
  (argsCompNames, argsCompC) <- compArgs args
  -- free r4, it will be used as empty register to dump args from 04-
  isUsedR4 <- inUseReg Arm.R4
  (_, mvR4ToStack) <- if isUsedR4
                        then moveRegToStack Arm.R4
                        else return ("", [])
  -- load compiled values of arguments 0-3 to registers R0 - R3
  arg0To3LdC <- loadArgsToRegs (take 4 argsCompNames) [Arm.R0, Arm.R1, Arm.R2, Arm.R3]
  -- dump current state of scratch registers: r0-r3 and r12
  regsToDumpRest <- inUseRegs [ Arm.R0, Arm.R1, Arm.R2, Arm.R3 ]
  let scratchRegDumpC = if length regsToDumpRest == 0
                          then []
                          else [ Arm.STM Arm.FD Arm.SP updateReg regsToDumpRest ]
  -- save to stack compiled values of arguments from 4 using empty register R4
  -- none of r0-r3 will be touched
  argFrom4LdC <- loadArgsToStack (drop 4 argsCompNames) Arm.R4
  -- call fun, always result is in R0
  let callFunC = [ Arm.BL Arm.CondNO (cleanName ident) ]
  -- result in R0 move to other register
  (retName, retReg, retCode) <- getRegWithNewVar Nothing [ Arm.R0 ]
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
          argsCompC ++
          mvR4ToStack ++
          arg0To3LdC ++
          scratchRegDumpC ++
          argFrom4LdC ++
          callFunC ++
          retCode ++
          mvValR0ToRet ++
          restStackC ++
          restRegC ++
          [ Arm.Comment $ "call end function " ++ ident ])
  where
    compArgs :: [SchemeInst] -> Env ([String], ArmCode)
    compArgs [] = return ([], [])
    compArgs (arg:args) = do
      (compName, compReg, compCode) <- compile arg
      (restNames, restCode) <- compArgs args
      return ([compName] ++ restNames,
              [ Arm.Comment $ "preparing arg " ++ compName ] ++
                compCode ++
                restCode)
    loadArgsToRegs :: [String] -> [Arm.Register] -> Env ArmCode
    loadArgsToRegs [] [] = return []
    loadArgsToRegs vns [] = return []
    loadArgsToRegs [] regs = return []
    loadArgsToRegs (vn:vns) (reg:regs) = do
      (_, code) <- loadVarToRegister vn (Just reg) []
      restC <- loadArgsToRegs vns regs
      return $ [ Arm.Comment $ "loading to reg arg " ++ (show $ length (vn:vns)) ] ++
               code ++ restC
    loadArgsToStack :: [Name] -> Arm.Register -> Env ArmCode
    loadArgsToStack [] _ = return []
    loadArgsToStack (vn:vns) emReg = do
      (regState, varState, stackPtr, some) <- get
      let code = case Maybe.fromJust ( Map.lookup vn varState ) of
                  LocReg reg -> [ Arm.STM Arm.FD Arm.SP updateReg [reg] ]
                  LocStack pos -> [ Arm.LDR emReg Arm.FP (Arm.O2ShImm pos) Arm.Offset ] ++
                                  [ Arm.STM Arm.FD Arm.SP updateReg [emReg] ]
      rest <- loadArgsToStack vns emReg
      -- reverse order of arugments
      return $ rest ++
               [ Arm.Comment $ "loading to stack arg " ++ (show $ 4 - length (vn:vns)) ] ++ 
               code

defineFun ident args body metal = do
  -- reset env
  resetEnv
  -- add definitions of function's arguments to environment
  addFunArgsToEnv args
  -- label for function
  let funLabel = [ Arm.Label (cleanName ident) ]
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

  return $ if metal
            then  [ Arm.Inline $ ".type " ++ (cleanName ident) ++ ", %function" ] ++
                  funLabel ++
                  [ Arm.Comment "define metal" ] ++
                  [ Arm.Comment "metal start" ] ++
                  bodyC ++
                  [ Arm.Comment "metal end" ]
            else  [ Arm.Inline $ ".type " ++ (cleanName ident) ++ ", %function" ] ++
                  funLabel ++
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
      (compName, compReg, compCode) <- compile body
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
compile (List (Atom "comment" : String cm : [])) = return ("", Arm.NoReg, [Arm.Comment cm])

-- Inline
compile (List (Atom "inline" : String inl : [])) = return ("", Arm.NoReg, [Arm.Inline inl])

-- Function
compile (List (Atom "define" : List (Atom name : args) : body : [])) = do
  let argNames = unArgs args
  funCode <- defineFun name argNames body False
  return ("", Arm.NoReg, funCode)

  where
    unArgs [] = []
    unArgs (Atom atom : args) = [ atom ] ++ unArgs args

compile (List (Atom "define-metal" : List (Atom name : args) : body : [])) = do
  let argNames = unArgs args
  funCode <- defineFun name argNames body True
  return ("", Arm.NoReg, funCode)

  where
    unArgs [] = []
    unArgs (Atom atom : args) = [ atom ] ++ unArgs args

-- Constants
compile (Number num) = do
  (name, reg, code) <- getRegWithNewVar Nothing []
  return $ (name, reg, 
            [ Arm.Comment $ "int " ++ (show num) ++ " with name " ++ name ] ++
            code ++
            [ Arm.MOV reg (Arm.O2ShImm num) ] ++
            tagReg reg TInt)

compile (Bool isTrue) = do
  (name, reg, code) <- getRegWithNewVar Nothing []
  let boolToInt = if isTrue
                    then 1
                    else 0
  return $ (name, reg, 
            [ Arm.Comment $ "bool " ++ (show isTrue) ++ " with name " ++ name ] ++
            code ++
            [ Arm.MOV reg (Arm.O2ShImm boolToInt) ] ++
            tagReg reg TBool)


-- Empty list
compile (List (Atom "quote" : List [] : [])) = do
  (name, reg, code) <- getRegWithNewVar Nothing []
  return $ (name, reg, 
            [ Arm.Comment $ "empty list" ++ " with name " ++ name ] ++
            code ++
            [ Arm.MOV reg (Arm.O2ShImm 0) ])


-- Variable
compile (Atom varName) = do
  (reg, code) <- loadVarToRegister varName Nothing []
  if reg == Arm.NoReg
    -- assume var is a name of process
    -- used in closures
    then do
      (_, freeC) <- moveRegToStack Arm.R0
      setVarToReg varName Arm.R0
      return (varName, Arm.R0, freeC ++
                               [ Arm.Inline $ "ADR R0, " ++ (cleanName varName) ])
    else return (varName, reg, code)


-- sequence of expression
compile (List (Atom "begin" : blocks)) = do
  compBlocks blocks
  where
    compBlocks [] = return ("", Arm.NoReg, [])
    compBlocks (b:[]) = do
      (name, reg, code) <- compile b
      return (name, reg, code)
    compBlocks (b:bs) = do
      (name, reg, code) <- compile b
      (nameBs, regBs, codeBs) <- compBlocks bs
      return (nameBs, regBs, code ++ codeBs)


-- boolean operations
compile (List (Atom "if" : pred : trueBlock : falseBlock : [])) = do
  -- compile predicate
  (predName, predReg, predC) <- compile pred

  -- labels
  labelThen <- createLabel "then"
  labelElse <- createLabel "else"
  labelEnd <- createLabel "end"

  -- compile blocks
  -- return register
  (retName, retReg, retRegC) <- getRegWithNewVar Nothing [ predReg ]
  -- dump all registers
  -- so we can store state of the stack (current position)
  -- and all vars are on the stack
  beforeDumpC <- dumpUsedRegs Arm.NoReg

  -- get starting state
  -- save state of current vars and regs
  -- it is necessary to restore it before else is executed
  -- the state after then and else block must be the same
  (envReg, envVar, envStackPtr, envCountPtr) <- get

  -- compile true block
  (trueBName, trueBReg, trueBC) <- compile trueBlock
  -- load retName to previously decided return register retReg
  (trueRetLdReg, trueRetLdC) <- loadVarToRegister retName (Just retReg) [ trueBReg ]
  -- again load result because it might have been in register retReg
  (trueBLdReg, trueBLdC) <- loadVarToRegister trueBName Nothing [ retReg ]
  -- finally move values
  let trueMoveC = [ Arm.MOV retReg (Arm.O2ShReg trueBLdReg Arm.NoShift) ]
  -- dump regs
  trueDumpC <- dumpUsedRegs Arm.NoReg
  let trueResetSP = [ Arm.MOV Arm.SP (Arm.O2ShReg Arm.FP Arm.NoShift),
                      Arm.Sub Arm.SP Arm.SP (Arm.O2ShImm envStackPtr) ]
  -- we returned to the state before executing then block
  -- restoring stack position to before executing then block
  -- is ok, because stack might have grown only with temporary vars
  -- that were used during block execution

  -- compile else block
  -- restore state
  (envReg2, envVar2, envStackPtr2, envCountPtr2) <- get
  -- restore stack position
  put (envReg, envVar, envStackPtr, envCountPtr2)

  -- compile
  (falseBName, falseBReg, falseBC) <- compile falseBlock
  -- load retName to previously decided return register retReg
  (falseRetLdReg, falseRetLdC) <- loadVarToRegister retName (Just retReg) [ falseBReg ]
  -- again load result because it might have been in register retReg
  (falseBLdReg, falseBLdC) <- loadVarToRegister falseBName Nothing [ retReg ]
  -- finally move values
  let falseMoveC = [ Arm.MOV retReg (Arm.O2ShReg falseBLdReg Arm.NoShift) ]

  -- restore state before executing else block (and whole if-then-else block)
  -- dump regs
  falseDumpC <- dumpUsedRegs Arm.NoReg
  let falseResetSP = [ Arm.MOV Arm.SP (Arm.O2ShReg Arm.FP Arm.NoShift),
                      Arm.Sub Arm.SP Arm.SP (Arm.O2ShImm envStackPtr) ]
  (envReg3, envVar3, envStackPtr3, envCountPtr3) <- get
  put (envReg, envVar, envStackPtr, envCountPtr3)

  return (retName, retReg,
          [ Arm.Comment " -- if-then-else -- " ] ++
          [ Arm.Comment "calculating predicate" ] ++
          predC ++
          retRegC ++
          beforeDumpC ++
          [ Arm.CMP predReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondNE labelElse ] ++
          [ Arm.Label labelThen ] ++
          [ Arm.Comment "then block" ] ++
          trueBC ++
          trueRetLdC ++
          [ Arm.Comment "then reg move" ] ++
          trueBLdC ++
          trueMoveC ++
          [ Arm.Comment "then dump regs" ] ++
          trueDumpC ++
          [ Arm.Comment "then reset SP" ] ++
          trueResetSP ++
          [ Arm.B Arm.CondNO labelEnd ] ++
          [ Arm.Label labelElse ] ++
          [ Arm.Comment "else block" ] ++
          falseBC ++
          falseRetLdC ++
          [ Arm.Comment "else reg move" ] ++
          falseBLdC ++
          falseMoveC ++
          [ Arm.Comment "else dump regs" ] ++
          falseDumpC ++
          [ Arm.Comment "else reset SP" ] ++
          falseResetSP ++
          [ Arm.Label labelEnd ])
  where
    dumpUsedRegs retReg = do
      usedRegs <- inUseRegs [Arm.R0, Arm.R1, Arm.R2, Arm.R3, Arm.R4,
                             Arm.R5, Arm.R6, Arm.R7, Arm.R8, Arm.R9]
      dumpRegs usedRegs
      where
        dumpRegs [] = return []
        dumpRegs (reg:regs) = do
          (name, code) <- if reg == retReg
                            then return ("", [])
                            else moveRegToStack reg
          restCode <- dumpRegs regs
          return $ code ++ restCode

compile (List (Atom "not" : pred : [])) = do
  -- compile predicate
  (predName, predReg, predC) <- compile pred

  -- labels
  labelTrue <- createLabel "true"
  labelFalse <- createLabel "false"
  labelEnd <- createLabel "end"

  (retName, retReg, retRegC) <- getRegWithNewVar Nothing [ predReg ]


  return (retName, retReg,
          [ Arm.Comment " -- not -- " ] ++
          [ Arm.Comment "calculating predicate" ] ++
          predC ++
          retRegC ++
          [ Arm.CMP predReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondNE labelFalse ] ++
          [ Arm.Label labelTrue ] ++
          [ Arm.MOV retReg (Arm.O2ShImm falseTagged) ] ++
          [ Arm.B Arm.CondNO labelEnd ] ++
          [ Arm.Label labelFalse ] ++
          [ Arm.MOV retReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.Label labelEnd ])


compile (List (Atom "and" : pred1 : pred2 : [])) = do
  -- compile predicate
  (pred1Name, pred1Reg, pred1C) <- compile pred1
  (pred2Name, pred2Reg, pred2C) <- compile pred2

  -- labels
  labelTrue <- createLabel "true"
  labelFalse <- createLabel "false"
  labelEnd <- createLabel "end"

  (retName, retReg, retRegC) <- getRegWithNewVar Nothing [ pred1Reg, pred2Reg ]


  return (retName, retReg,
          [ Arm.Comment " -- and -- " ] ++
          [ Arm.Comment "calculating predicates" ] ++
          pred1C ++
          pred2C ++
          retRegC ++
          [ Arm.CMP pred1Reg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondNE labelFalse ] ++
          [ Arm.CMP pred2Reg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondNE labelFalse ] ++
          [ Arm.Label labelTrue ] ++
          [ Arm.MOV retReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondNO labelEnd ] ++
          [ Arm.Label labelFalse ] ++
          [ Arm.MOV retReg (Arm.O2ShImm falseTagged) ] ++
          [ Arm.Label labelEnd ])


compile (List (Atom "or" : pred1 : pred2 : [])) = do
  -- compile predicate
  (pred1Name, pred1Reg, pred1C) <- compile pred1
  (pred2Name, pred2Reg, pred2C) <- compile pred2

  -- labels
  labelTrue <- createLabel "true"
  labelFalse <- createLabel "false"
  labelEnd <- createLabel "end"

  (retName, retReg, retRegC) <- getRegWithNewVar Nothing [ pred1Reg, pred2Reg ]


  return (retName, retReg,
          [ Arm.Comment " -- or -- " ] ++
          [ Arm.Comment "calculating predicates" ] ++
          pred1C ++
          pred2C ++
          retRegC ++
          [ Arm.CMP pred1Reg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondEQ labelTrue ] ++
          [ Arm.CMP pred2Reg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.B Arm.CondEQ labelTrue ] ++
          [ Arm.Label labelFalse ] ++
          [ Arm.MOV retReg (Arm.O2ShImm falseTagged) ] ++
          [ Arm.B Arm.CondNO labelEnd ] ++
          [ Arm.Label labelTrue ] ++
          [ Arm.MOV retReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.Label labelEnd ])


-- basic binary operations
compile (List (Atom "+" : arg1 : arg2 : [])) = do
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Add retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  
compile (List (Atom "-" : arg1 : arg2 : [])) =
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Sub retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  
compile (List (Atom "*" : arg1 : arg2 : [])) =
  compileBinOp (\retReg reg1 reg2 -> [ Arm.Mul retReg reg1 (Arm.O2ShReg reg2 Arm.NoShift) ]) arg1 arg2
  

-- basic binary comparisons
compile (List (Atom "=" : arg1 : arg2 : [])) = do
  compileBinComp Arm.CondEQ arg1 arg2
  
compile (List (Atom "<" : arg1 : arg2 : [])) =
  compileBinComp Arm.CondLT arg1 arg2
  
compile (List (Atom "<=" : arg1 : arg2 : [])) =
  compileBinComp Arm.CondLE arg1 arg2
  
compile (List (Atom ">" : arg1 : arg2 : [])) =
  compileBinComp Arm.CondGT arg1 arg2
  
compile (List (Atom ">=" : arg1 : arg2 : [])) =
  compileBinComp Arm.CondGE arg1 arg2
 
-- compile directly function in assembler
compile (List (Atom "assembler" : blocks )) = do
  let (List (Atom funName : args ) ) = blocks !! 0
  (Arm.Inline funDecl : rest) <- comp (take 1 blocks)
  let body = drop 1 blocks
  bodyC <- comp body
  return ("", Arm.NoReg, 
          [ Arm.Inline $ ".type " ++ (cleanName funName) ++ ", %function" ] ++
          [ Arm.Label $ cleanName funName ] ++
          [ Arm.Comment $ "def: " ++ funDecl ] ++
          bodyC)
  where
    comp [] = return $ Arm.Inline "" : []
    comp (List (Atom "comment" : String cm : []) : rest ) = do
      restSt <- comp rest
      return $ Arm.Comment cm : restSt
    comp (List (Atom "#" : args) : rest ) = do
      restSt <- comp rest
      return $ Arm.Comment ("# " ++ (show args)) : restSt
    comp (List inst:rest) = do
      let unInst = map (\(Atom a) -> a) inst
      let instSt = Arm.Inline $ foldl (\s a -> s ++ " " ++ a) "" unInst
      restSt <- comp rest
      return $ instSt : restSt

 
-- let block
compile (List (Atom "let" : List varBlock : codeBlock : [] )) = do
  varBlockC <-compileVarBlock varBlock
  (codeBlockVar, codeBlockReg, codeBlockC) <- compileCodeBlock codeBlock
  return (codeBlockVar, codeBlockReg, 
          [ Arm.Comment "let block start" ] ++ 
          varBlockC ++ codeBlockC ++
          [ Arm.Comment "let block end" ])

  where
    compileVarBlock [] = return []
    compileVarBlock (List (Atom varName : varVal : []) : varRest) = do
      (varN, varR, varC) <- compile varVal
      (name, reg, regC) <- getRegWithNewVar (Just varName) [varR]
      restC <- compileVarBlock varRest
      return $ [ Arm.Comment "let block var def start" ] ++
               varC ++ regC ++
               [ Arm.MOV reg (Arm.O2ShReg varR Arm.NoShift) ] ++ 
               [ Arm.Comment "let block var def end" ] ++
               restC
    compileCodeBlock block = compile block

      
      


-- comment
compile (List (Atom "#" : args)) = do
  return ("", Arm.NoReg, [])

-- If it was not recognized it must be a function
compile (List (Atom name : args)) = do
  callFun name args


compile (List []) = do
  (retName, retReg, retRegC) <- getRegWithNewVar Nothing []
  return (retName, retReg,
          [ Arm.Comment "List []" ] ++
          retRegC ++
          [ Arm.MOV retReg (Arm.O2ShImm 0) ])

compile (EmptyVal) = do
  return ("", Arm.NoReg, [])

compile inst = do
  return ("", Arm.NoReg, [Arm.Comment $ "not recognised: " ++ (show inst)])



-- Basic binary operations
compileBinOp :: (Arm.Register -> Arm.Register -> Arm.Register -> ArmCode) -> 
                 SchemeInst -> SchemeInst -> Env (Name, Arm.Register, ArmCode)
compileBinOp binOp arg1 arg2 = do
  -- compile arugments
  (arg1Name, arg1Reg, arg1C) <- compile arg1
  (arg2Name, arg2Reg, arg2C) <- compile arg2

  (retName, retReg, retRegC) <- getRegWithNewVar Nothing []
  (arg1LdReg, arg1LdC) <- loadVarToRegister arg1Name Nothing [ retReg ]
  (arg2LdReg, arg2LdC) <- loadVarToRegister arg2Name Nothing [ retReg, arg1LdReg ]

  -- untag
  let arg1UntagC = untagReg arg1LdReg TInt
  -- if arguments are the same don't untag two times
  -- the same register
  let arg2UntagC = if arg1Name == arg2Name && arg1LdReg == arg2LdReg
                    then []
                    else untagReg arg2LdReg TInt

  let addC = binOp retReg arg1LdReg arg2LdReg

  -- tag result
  let addResTagC = tagReg retReg TInt

  -- tag args
  let arg1TagC = tagReg arg1LdReg TInt
  let arg2TagC = if arg1Name == arg2Name && arg1LdReg == arg2LdReg
                    then []
                    else tagReg arg2LdReg TInt

  return (retName, retReg,
          arg1C ++
          arg2C ++
          retRegC ++
          arg1LdC ++
          arg2LdC ++
          arg1UntagC ++
          arg2UntagC ++
          addC ++
          addResTagC ++
          arg1TagC ++
          arg2TagC)

-- Basic binary comparisons
compileBinComp :: Arm.Condition -> SchemeInst -> SchemeInst -> Env (Name, Arm.Register, ArmCode)
compileBinComp cond arg1 arg2 = do
  -- compile arugments
  (arg1Name, arg1Reg, arg1C) <- compile arg1
  (arg2Name, arg2Reg, arg2C) <- compile arg2

  (retName, retReg, retRegC) <- getRegWithNewVar Nothing []
  (arg1LdReg, arg1LdC) <- loadVarToRegister arg1Name Nothing [ retReg ]
  (arg2LdReg, arg2LdC) <- loadVarToRegister arg2Name Nothing [ retReg, arg1LdReg ]

  --
  -- skipping tagging and untagging because they have the same tag
  --

  labelTrue <- createLabel "true"
  labelFalse <- createLabel "false"
  labelEnd <- createLabel "end"

  return (retName, retReg,
          [ Arm.Comment $ " -- comp " ++ (show cond) ++ " -- " ] ++
          arg1C ++
          arg2C ++
          retRegC ++
          arg1LdC ++
          arg2LdC ++
          [ Arm.CMP arg1LdReg (Arm.O2ShReg arg2LdReg Arm.NoShift) ] ++
          [ Arm.B cond labelTrue ] ++
          [ Arm.Label labelFalse ] ++
          [ Arm.MOV retReg (Arm.O2ShImm falseTagged) ] ++
          [ Arm.B Arm.CondNO labelEnd ] ++
          [ Arm.Label labelTrue ] ++
          [ Arm.MOV retReg (Arm.O2ShImm trueTagged) ] ++
          [ Arm.Label labelEnd ])


--------------------------------------------------------------------------------
-- Starting compiler

addRegisters :: Env ()
addRegisters = do
  let allRegs = Map.fromList [free Arm.R0, free Arm.R1, free Arm.R2, free Arm.R3,
                              free Arm.R4, free Arm.R5, free Arm.R6, free Arm.R7,
                              free Arm.R8, free Arm.R9]
  (regState, varState, stackPtr, some) <- get
  let newRegState = Map.union regState allRegs -- already specified reg state is mort important
  put (newRegState, varState, stackPtr, some)

  where
    free reg = (reg, Nothing)

programHeader = []
  
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
      (funName, funReg, funCode) <- compile fun
      restCode <- compBody rest
      return $ funCode ++ restCode


--------------------------------------------------------------------------------
-- TO DO

-- built in function, list? etc
-- adding info about VAR type to state
-- multi argument (+ 1 2 3 4 5)
-- tail recursion
-- let expression
-- do expression

-- equality
-- eq? It compares addresses of two objects and returns #t if they are same.
-- eqv? It compares types and values of two object stored in the memory space. 
-- equal? It is used to compare sequences such as list or string.
-- cond
-- write & read

-- type checking
-- pair? It returns #t if the object consists of cons cells (or a cons cell).
-- list? It returns #t if the object is a list. Be careful in that '() is a list but not a pair.
-- null? It returns #t if the object is '().
-- symbol? It returns #t if the object is a symbol.
-- char? It returns #t if the object is a character. 
-- string? It returns #t if the object is a string. 
-- number? It returns #t if the object is a number.
-- complex? It returns #t if the object is a complex number.
-- real? It returns #t if the object is a real number
-- rational? It returns #t if the object is a rational number.
-- integer? It returns #t if the object is an integral
-- exact? It returns #t if the object is not a floating point number.
-- inexact? It returns #t if the object is a floating point number.

-- higher order function
-- sort: (sor '(1 2 3 4) <)
-- map: (map + '(1 2 3) '(4 5 6))
-- for-each: (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3 4))
--------------------------------------------------------------------------------
-- Errors and limitations

