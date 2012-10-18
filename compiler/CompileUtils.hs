module CompileUtils where

import Prelude
import Control.Monad
import Control.Monad.State
import System.Environment
import System.IO
import System.IO.Unsafe
import System.FilePath

import qualified Data.Map as Map

import Parser
import PreCompiler
import Compiler
import ArmInstructions

-- printing nicely generated code
formatCompCode ([]) = ""
formatCompCode (code:rest) =
  let codeF = case code of
                Label l -> line (show code)
                Special s -> indentLine (show code)
                _ -> indentLine (show code)
  in codeF ++ (formatCompCode rest)

  where
    indentLine l = "  " ++ l ++ "\n"
    line l = l ++ "\n"

compile :: String -> Either String [ArmInstruction]
compile code = do
  case parseCode code of
    Left error -> Left ("Error, parsing: \n" ++ show error)
    Right code -> do
      let preCompiledCode = preCompileCode code
      let compiled = evalState (compileCode preCompiledCode) (Map.fromList [], Map.fromList [], 0, (-1, -1))
      Right compiled

compileFile fileName = do
  let outputFileName = replaceExtension fileName ".s"
  contents <- readFile fileName

  let compiled = CompileUtils.compile contents
  case compiled of
    Left str -> putStrLn str
    Right insts ->
       writeFile outputFileName $ formatCompCode insts
