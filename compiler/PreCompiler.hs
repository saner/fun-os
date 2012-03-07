module PreCompiler where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Maybe
import Debug.Trace


import qualified ArmInstructions as Arm
import SchemeDataTypes


-- scanning for c function declarations
-- replacing calls to c functions
-- with functions' wrappers

-- represents function declaration
-- function-name, return-type, arg-types
data DeclCFun = DeclCFun String SchemeType [SchemeType]

-- find find-c-fun declarations
findRemoveDeclCFun :: SchemeInst -> (SchemeInst, [DeclCFun])

findRemoveDeclCFun (List (Atom "decl-c-fun" : (Atom funName) : retType : List argTypes : [])) = 
  (EmptyVal, [DeclCFun funName (mapType retType) (map mapType argTypes)])
  where
    mapType (Atom "int") = TInt
    mapType (Atom "bool") = TBool
    mapType (Atom "void") = TVoid
    mapType _ = TVoid

findRemoveDeclCFun (List insts) = 
  let instsDecls = map findRemoveDeclCFun insts
      frInsts = List $ map fst instsDecls
      frDecls = concat $ map snd instsDecls
  in (frInsts, frDecls)

findRemoveDeclCFun (DottedList insts inst) = 
  let instsDecls = map findRemoveDeclCFun insts
      instDecl = findRemoveDeclCFun inst
      frInsts = DottedList (map fst instsDecls) (fst instDecl)
      frDecls = (concat $ map snd instsDecls) ++ (snd instDecl)
  in (frInsts, frDecls)

-- if was not matched so don't transform instruction
findRemoveDeclCFun inst = (inst, []) 

-- changes all C functions names to their wrappers
changeToWrapperName funs (List (Atom funName : rest)) = 
  let name = if any (\(DeclCFun name _ _) -> name == funName) funs
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
wrapName name = name ++ "_c_wrapper"

declCFun code = 
  let instsDecls = map findRemoveDeclCFun code
      insts = map fst instsDecls
      funs = concat $ map snd instsDecls
      funsCode = map genFun funs
      instsWrapper = map (changeToWrapperName funs) insts
  in (instsWrapper ++ 
      [ List [ Atom "comment", String "C functions wrappers" ] ] ++ funsCode)
  where
    genFun (DeclCFun funName retType argTypes) = 
      let argsStackUntagged = untagStackArgs argTypes
          argsRegUntagged =  concat $ map untag (zip argTypes [Arm.R0, Arm.R1, Arm.R2, Arm.R3])
          taggedRet = tag (retType, Arm.R0)
          wName = wrapName funName
          body = [ List [ Atom "comment", String "body" ] ] ++
                 argsStackUntagged ++
                 argsRegUntagged ++
                 [ List [ Atom $ "BL " ++ funName ] ] ++
                 taggedRet ++
                 [ List [ Atom $ "BX LR" ] ]
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
            List [ Atom $ "LSR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 2) ]]
        untag (TBool, reg) =
          [ List [ Atom "comment" , String ("untagging int, reg " ++ (show reg)) ],
            List [ Atom $ "LSR " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 3) ]]
        untag _ = []

        tag (TInt, reg) =
          [ List [ Atom "comment" , String ("tagging int, reg " ++ (show reg)) ],
            List [ Atom $ "LSL " ++ (show reg) ++ ", " ++ (show reg) ++ ", #" ++ (show 2) ],
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


-- running pre-compiler
preCompileCode :: [SchemeInst] -> [SchemeInst]
preCompileCode [] = []
preCompileCode code =
  let finalCode = declCFun condExpanded
      condExpanded = map condExpansion code
  in finalCode
