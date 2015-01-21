module Language.Exec where

import qualified Data.Map as M
import Language.Expressions
import Data.Maybe

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState
-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String
-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command
-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
, wd :: FilePath
, vartable :: VarTable
} deriving Show

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram ct (Left fp) exprs = runHashProgram ct (Right (ScriptState "" fp M.empty)) exprs
runHashProgram _ (Right ss) [] = return ss
runHashProgram ct (Right ss) (topExpr:exprs) = do
	newSS <- runTopLevel ct ss topExpr
	runHashProgram ct (Right newSS) exprs

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct ss (TLCmd cmd) = do
	newSS <- executeCommand cmd ct ss
	return newSS

runTopLevel _ _ _ = undefined

vtLookup ::VarTable -> Expr -> String
vtLookup _ (Str s) = s
vtLookup vt (Var s) = fromMaybe (error "variable " ++ s ++ " does not exist\n") $ M.lookup s vt

varValToStr :: Expr -> String
varValToStr (Str s) = s
varValToStr (Var s) = s

executeCommand :: Cmd -> CommandTable -> ScriptState -> IO ScriptState
executeCommand (Cmd cmdName argums input out isApp) ct ss@(ScriptState _ _ vt) =  do
	let getExpr = vtLookup vt
	let cmdName' = getExpr cmdName
	let cmd = fromMaybe (error ("command " ++ cmdName' ++ " does not exist\n")) $ M.lookup cmdName' ct
	fromFile <- maybe (return []) readFile (fmap getExpr input)
	newSS <- cmd ((map getExpr argums) ++ (if fromFile == "" then [] else [fromFile])) ss
	if (isNothing out) then putStrLn $ output newSS else ((if isApp then appendFile else writeFile) (getExpr $ fromJust out) $ output newSS)
	return newSS

executeCommand (Assign varName value) _ ss@(ScriptState _ _ vt) = return ss{vartable = M.insert (varValToStr varName) (varValToStr value) vt}
