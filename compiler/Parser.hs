module Parser where

import SchemeDataTypes

import Text.ParserCombinators.Parsec
import Control.Monad

-- TODO
-- (cos 1 2 3)  - funkcja + argumenty
-- (quote 2 2)
-- '(1 2 3) 
-- (list 1 2 3)

symbol :: Parser Char
symbol = oneOf "!#$%&|+-*/:<>=?@^_~,."


parseString :: Parser SchemeInst
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x


parseAtom :: Parser SchemeInst
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let ident = first:rest
  return $ case ident of
              "#t" -> Bool True
              "#f" -> Bool False
              _ -> Atom ident


parseNumber :: Parser SchemeInst
parseNumber = liftM (Number . read) $ many1 digit


parseListContent :: Parser [SchemeInst]
parseListContent = sepBy parseExpr spaces

parseList :: Parser SchemeInst
parseList = do
  content <- parseListContent
  return $ List content


parseDottedListContent :: Parser ([SchemeInst], SchemeInst)
parseDottedListContent = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return (head, tail)

parseDottedList :: Parser SchemeInst
parseDottedList = do
  (head, tail) <- parseDottedListContent
  return $ DottedList head tail


parseQuoteShort :: Parser SchemeInst
parseQuoteShort = do
  char '\''
  x <- parseExpr
  rest <- parseExpr <|> (return EmptyVal)
  if rest == EmptyVal
    then return $ List [Atom "quote" , x]
    else return $ List [Atom "quote" , x, rest]

parseExpr :: Parser SchemeInst
parseExpr =   parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoteShort
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


readExpr :: String -> String
readExpr input =
  case parse parseExpr "scheme" input of
  Left err -> "Parser error: " ++ show err
  Right res -> "Parser works: " ++ show res

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> string " "

parseExpr2 = do
  exp <- parseExpr
  eol
  return exp

parseBody :: Parser [SchemeInst]
parseBody = do
  exp <- sepBy parseExpr2 (many eol)
  eof
  return exp

readBody :: String -> String
readBody input =
  case parse parseBody "scheme" input of
  Left err -> "Parser error: " ++ show err
  Right res -> "Parser works: " ++ show res

parseCode input = parse parseBody "scheme" input
