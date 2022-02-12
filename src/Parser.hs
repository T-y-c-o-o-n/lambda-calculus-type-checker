{-# LANGUAGE TupleSections #-}

module Parser where

import Base
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (symbol)
import Data.Map (fromList)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) TypeInference
parse = Text.Megaparsec.parse (parseTypeInference <* eof) ""

char :: Char -> Parser ()
char c = void $ symbol C.space [c]

string :: String -> Parser ()
string = void . symbol C.space

parseTypeInference :: Parser TypeInference
parseTypeInference = do
  C.space
  ctx <- parseContext
  string "|-"
  term <- parseTerm
  char ':'
  t <- parseType
  return (ctx, term, t)

parseContext :: Parser Context
parseContext =
  fromList <$> sepBy
    ( do
        x <- parseVariable
        char ':'
        (x,) <$> parseType
    )
    (char ',')

parseTerm :: Parser Term
parseTerm =
  do
    C.space
    foldl1 (:@)
      <$> some
        ( choice
            [ V <$> parseVariable,
              do
                char '('
                parseTerm <* char ')',
              do
                char '\\'
                x <- parseVariable
                char ':'
                t <- parseType
                char '.'
                L x t <$> parseTerm
            ]
        )

parseType :: Parser Type
parseType =
  do
    C.space
    foldr1 (:=>)
      <$> sepBy
        ( choice
            [ T <$> parseVariable,
              do
                char '('
                parseType <* char ')'
            ]
        )
        (string "->")

parseVariable :: Parser String
parseVariable =
  do
    C.space
    c <- C.lowerChar
    s <- many (C.lowerChar <|> C.digitChar <|> C.char '\'')
    C.space
    return $ c : s
