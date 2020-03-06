module Parser where

import qualified PiCalculusTypes as PC 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
   emptyDef { Token.identStart      = letter <|> oneOf "!?"
            , Token.identLetter     = alphaNum
            , Token.opLetter        = oneOf "!?|.*+"
            }

lexer = Token.makeTokenParser languageDef

-- specific token parsers
parens = Token.parens lexer -- parentheses
identifier =    (Token.identifier lexer)
            <|> (Token.operator lexer)
                -- identifier
whiteSpace = Token.whiteSpace lexer --  whitespace


