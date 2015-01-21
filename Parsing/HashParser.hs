module Parsing.HashParser where

import Text.Parsec
import Text.Parsec.String
import Language.Expressions
import Control.Applicative ((<*), (*>))
import Data.Maybe (isJust)

parseStart :: String -> [TLExpr]
parseStart p = case parse (many tlexpr) "parse error" p of
	Left _ -> error "parse error"
	Right t ->  t

tlexpr :: Parser TLExpr
tlexpr = parseCmds

parseString :: Parser String
parseString = many1 $ noneOf "/*=\" <>\n;"

comments :: Parser Cmd
comments = (try singleLine <|> multiLine) >> return ((Cmd $ Str "") [] Nothing Nothing False)

parseCmds :: Parser TLExpr
parseCmds = (((try cmd <|> try assign <|> comments)  <* spaces <* optional comments) >>= return . TLCmd)

assign :: Parser Cmd
assign = do
	varName <- parseVar
	spaces
	char '='
	spaces
	value <- parseVal
	char ';'
	return $ Assign{var = varName, val = value}

parseVar :: Parser Expr
parseVar = do
	a <- letter
	b <- many alphaNum
	return $ Var $ a:b

parseVal :: Parser Expr
parseVal = do
	a <- many1 alphaNum
	return $ Str a

singleLine :: Parser String
singleLine = (string "//") *> (many $ noneOf "\n")

--try this more
multiLine :: Parser String
multiLine = (string "/*") *> (manyTill anyChar $ try (string "*/"))

stringLiteral :: Parser Expr
stringLiteral = char '"' *> (many1 $ noneOf "\"") <* char '"' >>= return . Str
 
expr :: Parser Expr
expr = try (parseString >>= return . Str)

cmd :: Parser Cmd
cmd = do
	spaces
  	cmdName <- expr <* spaces
  	cmdArgs <- many ((try stringLiteral <|> expr) <* spaces)
  	from <- optionMaybe fromFile <*spaces
  	to <- optionMaybe toFile <* spaces
  	let (out, app) = case to of
  		Nothing          -> (Nothing, False)
  		Just (ex, isApp) -> (Just ex, isApp)
  	char ';'                                 
  	return (Cmd cmdName cmdArgs from out app)

stringToMaybe :: String -> Maybe String
stringToMaybe a
	| null a = Nothing
	| otherwise = Just a

fromFile :: Parser Expr
fromFile = do
  char '<'
  spaces
  ret <- expr
  return ret

toFile :: Parser (Expr, Bool)
toFile = do
  char '>'
  app <- optionMaybe $ char '>'
  spaces
  ret <- expr
  spaces
  return (ret, isJust app)
