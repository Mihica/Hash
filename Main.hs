module Main where

import System.Environment
import System.IO
import Hash
import System.FilePath

main :: IO ()
main = do
	args <- getArgs
	if null args
		then run stdin
		else if (takeExtension $ args !! 0) == ".hash"
			then do
				h <- openFile (args !! 0) ReadMode
				run h
				hClose h
			else error "Input file is not a .hash file"
