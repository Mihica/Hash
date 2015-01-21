module Hash where

import System.Directory
import Parsing.HashParser (parseStart)
import Language.Exec
import Language.Commands
import System.IO

run :: Handle -> IO ()
run h = do
	putStrLn "Welcome to Hash"
	input <- hGetContents h
	let exprs = parseStart input
	workDir <- getCurrentDirectory
	-- print exprs
	finalScriptState <- runHashProgram commandList (Left workDir) exprs
	putStrLn ""
