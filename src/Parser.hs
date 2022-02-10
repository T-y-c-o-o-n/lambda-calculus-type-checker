module Parser where

import Base
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (symbol)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) Term
parse = Text.Megaparsec.parse (parseTerm <* eof) ""

char :: Char -> Parser ()
char c = void $ symbol C.space [c]

string :: String -> Parser ()
string = void . symbol C.space

parseTerm :: Parser Term
parseTerm =
  do
    C.space
    foldl1 (:@)
      <$> many
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
    foldl1 (:=>)
      <$> sepBy
        ( choice
            [ T <$> parseVariable,
              do
                char '('
                parseType <* char ')'
            ]
        )
        (string "->")

parseVariable :: Parser Var
parseVariable =
  do
    c <- C.lowerChar
    s <- many (C.lowerChar <|> C.digitChar <|> C.char '\'')
    C.space
    return $ c : s
