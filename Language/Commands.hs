module Language.Commands where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char (toLower, intToDigit, ord)
import Language.Exec
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath
import Numeric

commandList :: M.Map String Command
commandList = M.fromList [("create", create), ("rm", rm), ("mv", mv), ("hexdump", hexdump), ("cat", cat), ("pwd", pwd), ("echo", echo), ("cp", cp), ("cd", cd), ("ls", ls), ("grep", grep) ,("", retSame)]

pwd :: Command
pwd _ ss@(ScriptState _ workDir _) = return $ ss {output = workDir}

echo :: Command
echo (h:_) ss@(ScriptState _ _ vt) = return $ ss {output = echoHelp h}
	where 
		echoHelp (f:s:[])   = if f == '$' then (fromMaybe (error "Var undefined") (M.lookup [s] vt)) else f : s : [] 
		echoHelp (f:s:rest) = if f == '$' then (fromMaybe (error "Var undefined") (M.lookup [s] vt)) ++ (echoHelp rest) else f : (echoHelp $ s:rest) 
		echoHelp s = s
echo [] _ = error "Too few arguments"

--doesnt really work
cp :: Command
cp [_] _ = error "Too few arguments"
cp [] _  = error "Too few arguments"
cp lst ss@(ScriptState _ workDir _) = (mapM_ cpHelp $ zip (init lst) (repeat $ last lst)) >> return ss
	where cpHelp (frm, src) = do
		dir <- doesDirectoryExist src
		case dir of
			True ->  copyFile (workDir </> frm) (workDir </> src </> frm)
			False ->  readFile frm >>= withFile src WriteMode . flip hPutStr

mv :: Command
mv (src:dst:_) ss = cp [src,dst] ss >> removeFile src >> return ss
mv _ _ = error "Not enough arguments"

rm :: Command
rm lst ss = mapM_ removeFile lst >> return ss

create :: Command
create lst ss = mapM (flip openFile WriteMode) lst >>= mapM_ hClose >> return ss

cd :: Command
cd [] ss = getHomeDirectory >>= return . (\x -> ss {wd = x})
cd [h] ss@(ScriptState _ workDir _) 
	| h == ".." = return $ ss{wd = takeDirectory workDir}
	| otherwise = doesDirectoryExist newDir >>= (\x -> if x then return ss {wd = newDir} else error "Directory doesn't exist") 
		where newDir = workDir </> h
cd _ ss = return ss

ls :: Command
ls [] ss@(ScriptState _ workDir _) = do
	a <- getDirectoryContents workDir
	return ss{output = L.intercalate "\n" a}
ls (h:_) ss@(ScriptState _ workDir _) = do
	a <- doesDirectoryExist newDir >>= (\x -> if x then getDirectoryContents newDir else error "Directory doesn't exist")
	return ss{output = L.intercalate "\n" a}
		where newDir = workDir </> h

cat :: Command
cat fs ss = catHelp fs ss{output = ""}
	where 
		catHelp (h:t) scst@(ScriptState _ workDir _) = do
			conts <- readFile $ workDir </> h
			catHelp t scst{output = output scst ++ conts}
		catHelp [] scst = return scst

retSame :: Command
retSame _ ss = return ss {output = ""}

grep :: Command
grep lst ss@(ScriptState _ _ _) = do
	exist <- doesFileExist file
	if exist 
		then do
			ret <- readFile file >>= return . resolveC . filter (resolveV . L.isInfixOf (resolveI pattern)) . 
						resolveIList . resolveN . resolveO
			return ss{output = L.intercalate "\n" ret}
		else error "file doesnt exist"
	where
		pattern = last $ init lst
		file = last lst
		flags = L.sort $ init $ init lst
		[c,v,i,o,n] = map (flip elem flags) ["-c", "-v", "-i", "-o", "-n"]
		resolveN x = (if n then map (\(f,s) -> show f ++ " " ++ s) . zip ([1..] :: [Int]) else (\f -> f)) x
		resolveO = if o then words else lines
		resolveI x = if i then map toLower x else x
		resolveIList x = if i then map (map toLower) x else x
		resolveV x 	| v = not x
					| otherwise = x
		resolveC x = if c then [show $ length x] else x

hexdump :: Command
hexdump (f:_) ss@(ScriptState _ workDir _) = (readFile $ workDir </> f) >>= return . map (L.intercalate " " . map intToHex) . lines >>= (\x -> return ss{output = unlines x})
	where intToHex x = showIntAtBase 16 intToDigit (ord x) ""

hexdump _ _ = error "too few arguments"
