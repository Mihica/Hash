module Hash where

import System.Directory
import Parsing.HashParser (parseStart)
import Language.Exec
import Language.Commands
import qualified Data.Map as M

runInline :: IO ()
runInline = do
	putStrLn "Welcome to Hash"
	run

run :: IO ()
run = do
	input <- readLn --getContents better
	let exprs = parseStart input
	workDir <- getCurrentDirectory
	print exprs
	finalScriptState <- runTopLevel commandList (ScriptState "" workDir M.empty) exprs
	putStrLn ""
