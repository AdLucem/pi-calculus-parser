module Parser where

import PiCalculusTypes
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
   emptyDef { Token.identStart      = letter <|> oneOf "!?"
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["null", "word", "!", "?"]
            }

lexer = Token.makeTokenParser languageDef

-- specific token parsers
parens = Token.parens lexer -- parentheses
identifier = Token.identifier lexer -- identifier
operator   = Token.operator lexer -- operator
reserved   = Token.reserved lexer -- reserved name
whiteSpace = Token.whiteSpace lexer --  whitespace
dot = Token.dot lexer
symbol = Token.symbol lexer "|"


processParser :: Parser Process
processParser =  nullParser
             <|> prefixParser
             <|> parallelParser

-- null process
nullParser :: Parser Process
nullParser = do
    reserved "word"
    name <- identifier
    return $ Lexeme name

prefixParser :: Parser Process
prefixParser = do
    channels <- parens channelListParser
    channelsOpt <- parens channelListParser
    process <- processParser
    return $ PiCalculusTypes.Prefix channels channelsOpt process

parallelParser :: Parser Process
parallelParser = do
    plist <- (sepBy1 processParser symbol)
    return $ Parallel plist

channelListParser :: Parser [Channel]
channelListParser = do
    chlist <- (sepBy1 channelParser dot)
    return chlist

-- channel parser
channelParser :: Parser Channel 
channelParser =  output
             <|> input
             <|> empty

output :: Parser Channel
output = do
    reserved "!"
    id <- identifier
    return $ Output id

input :: Parser Channel
input = do
    reserved "?"
    id <- identifier
    return $ Input id

empty :: Parser Channel
empty = do
    reserved "o"
    return $ Empty


parseString :: String -> Process
parseString str =
  case parse processParser "" str of
     Left e  -> error $ show e
     Right r -> r


