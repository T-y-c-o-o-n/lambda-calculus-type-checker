{-# LANGUAGE TupleSections #-}

module Parser where

import Base
import Control.Monad (void)
import Data.Map (fromList)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (symbol)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) TypeInference
parse = Text.Megaparsec.parse (parseTypeInference <* eof) ""

char :: Char -> Parser ()
char c = void $ symbol C.space [c]

string :: String -> Parser ()
string = void . symbol C.space

{-
Grammar:

<type> ::= <type-atom>
       | <type-atom> -> <type>
       | @ <variable> . <type>

<type-atom> ::= <variable>
              | ( type )

<term> ::= [<application>] \ <variable> : <type> . <term>
         | [<application>] /\ <variable> . <term>
         | <application>

<application> ::= <atom>
                | <application> <atom>
                | <application> ! <type>

("!" is to differ term application and type application,
 otherwise even "x y" will be ambiguous:
  "term x applied to term y" or "term x applied to type y"?)

<atom> ::= <variable>
         | ( <term> )

<variable> ::= [a-z] [a-z0-9']*

-}

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
parseContext = do
  C.space
  fromList
    <$> sepBy
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
              between (char '(') (char ')') parseTerm,
              do
                char '\\'
                x <- parseVariable
                char ':'
                t <- parseType
                char '.'
                L x t <$> parseTerm,
              do
                string "/\\"
                a <- parseVariable
                char '.'
                LL a <$> parseTerm
            ]
        )

parseType :: Parser Type
parseType =
  do
    C.space
    foldr1 (:=>)
      <$> sepBy1
        ( choice
            [ T <$> parseVariable,
              between (char '(') (char ')') parseType
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
