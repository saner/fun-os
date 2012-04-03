module PreCompiler where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Maybe
import Debug.Trace


import qualified ArmInstructions as Arm
import SchemeDataTypes
import CompUtils


updateReg = True
-- scanning for c function declarations
-- replacing calls to c functions
-- with functions' wrappers

-- represents function declaration
-- function-name, return-type, arg-types
data CFun = CFun String SchemeType [SchemeType]

-- find find-c-fun declarations
findRemoveCFun :: SchemeInst -> (SchemeInst, [CFun])

findRemoveCFun (List (Atom "c-fun" : (Atom funName) : retType : List argTypes : [])) = 
  (EmptyVal, [CFun funName (mapType retType) (map mapType argTypes)])
  where
    mapType (Atom "int") = TInt
    mapType (Atom "bool") = TBool
    mapType (Atom "void") = TVoid
    mapType _ = TVoid

findRemoveCFun (List insts) = 
  let instsDecls = map findRemoveCFun insts
      frInsts = List $ map fst instsDecls
      frDecls = concat $ map snd instsDecls
  in (frInsts, frDecls)

findRemoveCFun (DottedList insts inst) = 
  let instsDecls = map findRemoveCFun insts
      instDecl = findRemoveCFun inst
      frInsts = DottedList (map fst instsDecls) (fst instDecl)
      frDecls = (concat $ map snd instsDecls) ++ (snd instDecl)
  in (frInsts, frDecls)

-- if was not matched so don't transform instruction
findRemoveCFun inst = (inst, []) 

-- changes all C functions names to their wrappers
changeToWrapperName funs (List (Atom funName : rest)) = 
  let name = if any (\(CFun name _ _) -> name == funName) funs
                then wrapName funName
                else funName
      changeRest = map (\a -> changeToWrapperName funs a) rest
  in List (Atom name : changeRest)

changeToWrapperName funs (List insts) = 
  List $ map (changeToWrapperName funs) insts
changeToWrapperName funs (DottedList insts inst) = 
  DottedList (map (changeToWrapperName funs) insts) (changeToWrapperName funs inst)

-- if was not matched so don't transform instruction
changeToWrapperName funs inst = inst 


wrapName :: String -> String
wrapName name = (cleanName name) ++ "_c_wrapper"

declCFun code = 
  let instsDecls = map findRemoveCFun code
      insts = map fst instsDecls
      funs = concat $ map snd instsDecls
      funsCode = map genFun funs
      instsWrapper = map (changeToWrapperName funs) insts
  in (instsWrapper ++ 
      [ List [ Atom "comment", String "C functions wrappers" ] ] ++ funsCode)
  where
    genFun (CFun funName retType argTypes) = 
      let argsStackUntagged = untagStackArgs argTypes
          argsRegUntagged =  concat $ map untag (zip argTypes [Arm.R0, Arm.R1, Arm.R2, Arm.R3])
          taggedRet = tag (retType, Arm.R0)
          wName = wrapName funName
          body = [ List [ Atom "comment", String "body" ] ] ++
                 [ List [ Atom "STMFD SP!, {LR}"] ] ++
                 argsStackUntagged ++
                 argsRegUntagged ++
                 [ List [ Atom $ "BL " ++ (cleanName funName) ] ] ++
                 taggedRet ++
                 [ List [ Atom "LDMFD SP!, {LR}"] ] ++
                 [ List [ Atom "BX LR" ] ]
      in List (Atom "assembler" : List (Atom wName : []) : body )
      where
        untagStackArgs args =
          case drop 4 args of
            [] -> []
            args ->
              -- there are more than 4 args
              -- need to untag them
              -- all calculations are going to be on reg R0
              [ List [ Atom "comment", String "!!!!!! NOT SUPPORTED !!!!!!" ]]
        untag (TInt, reg) =
          [ List [ Atom "comment", String ("untagging int, reg " ++ (show reg)) ],
            List [ Atom $ "LSR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 3) ]]
        untag (TBool, reg) =
          [ List [ Atom "comment" , String ("untagging bool, reg " ++ (show reg)) ],
            List [ Atom $ "LSR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 3) ]]
        untag _ = []

        tag (TInt, reg) =
          [ List [ Atom "comment" , String ("tagging int, reg " ++ (show reg)) ],
            List [ Atom $ "LSL " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 3) ],
            List [ Atom $ "ORR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 2) ]]
        tag (TBool, reg) =
          [ List [ Atom "comment" , String ("tagging bool, reg " ++ (show reg)) ],
            List [ Atom $ "LSL " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 3) ],
            List [ Atom $ "ORR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 4) ]]
        tag _ = []

-- expanding cond
condExpansion :: SchemeInst -> SchemeInst

condExpansion (List (Atom "cond" : blocks)) = 
  compCond blocks
  where
    compCond [] = List []
    compCond ( List ( Atom "else" : thenB :[] ) : []) =
      condExpansion thenB
    compCond ( List (pred : thenB : []) : restB) =
      let compPred = condExpansion pred
          compThenB = condExpansion thenB
          compRestB = compCond restB
      in List (Atom "if" : compPred : compThenB : compRestB : [])

condExpansion (List insts) = 
  List $ map condExpansion insts
condExpansion (DottedList insts inst) = 
  DottedList (map condExpansion insts) (condExpansion inst)

-- if was not matched so don't transform instruction
condExpansion inst = inst 


-- function-name, return-type, arg-types
type GlobalFun = String

-- find global-fun declarations
findRemoveGlobalFun :: SchemeInst -> (SchemeInst, [GlobalFun])

findRemoveGlobalFun (List (Atom "global-fun" : Atom funName : [])) = 
  (EmptyVal, [ funName])

findRemoveGlobalFun (List insts) = 
  let instsDecls = map findRemoveGlobalFun insts
      frInsts = List $ map fst instsDecls
      frDecls = concat $ map snd instsDecls
  in (frInsts, frDecls)

findRemoveGlobalFun (DottedList insts inst) = 
  let instsDecls = map findRemoveGlobalFun insts
      instDecl = findRemoveGlobalFun inst
      frInsts = DottedList (map fst instsDecls) (fst instDecl)
      frDecls = (concat $ map snd instsDecls) ++ (snd instDecl)
  in (frInsts, frDecls)


-- if was not matched so don't transform instruction
findRemoveGlobalFun inst = (inst, []) 

globalFun code = 
  let instsDecls = map findRemoveGlobalFun code
      insts = map fst instsDecls
      funs = concat $ map snd instsDecls
      funsCode = map genFun funs
  in ( [ List [ Atom "comment", String "global functions" ] ] ++ 
         funsCode ++
         insts)
  where
    genFun funName = 
      List (Atom "inline" : String (".global " ++ (cleanName funName)) : [])


-- expand list construct
-- (list 1 2 3 4)
listExpansion :: [SchemeInst] -> [SchemeInst]

listExpansion ls = 
  map expand ls

  where
    expand (List (Atom "list" : insts)) = 
      let instsEx = map expand insts
          instsCons = foldl (\c e -> List [Atom "cons", e, c]) (List [Atom "quote", List []]) (reverse instsEx)
      in instsCons

    expand (List insts) = 
      List $ map expand insts

    expand (DottedList insts inst) = 
      let instsEx = map expand insts
          instEx = expand inst
      in DottedList instsEx instEx

    expand inst = inst


-- vector constructor
-- (vector 1 2 3 4)
vectorConstructor :: [SchemeInst] -> [SchemeInst]

vectorConstructor ls = 
  map (construct 0) ls

  where
    construct vecNo (List (Atom "vector" : insts)) = 
      let vecName = "vec_" ++ (show vecNo)
          instsEx = map (construct (vecNo + 1)) insts
          vecSize = length insts
          instsSet = vecSet vecName 0 instsEx
      in List [ Atom "begin", 
                List [ Atom "comment", String "vector constructor" ],
                List [ Atom "let", List [ List [ Atom vecName, List [ Atom "make-vector", Number vecSize ] ] ], 
                                   List ( [Atom "begin"] ++ instsSet ++ [Atom vecName]  ) ]]
      where
        vecSet vecName pos [] = []
        vecSet vecName pos (inst : insts) =
          List [Atom "vector-set!", Atom vecName, Number pos, inst] : vecSet vecName (pos + 1) insts


    construct vecNo (List insts) = 
      List $ map (construct vecNo) insts

    construct vecNo (DottedList insts inst) = 
      let instsEx = map (construct vecNo) insts
          instEx = construct vecNo inst
      in DottedList instsEx instEx

    construct vecNo inst = inst



-- running pre-compiler
preCompileCode :: [SchemeInst] -> [SchemeInst]
preCompileCode [] = []
preCompileCode code =
  let code1 = map condExpansion code
      code2 = declCFun code1
      code3 = globalFun code2
      code4 = listExpansion code3
      code5 = vectorConstructor code4
  in code5
