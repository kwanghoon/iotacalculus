module Token (Token(..), keywords) where

import TokenInterface

data Token =
    END_OF_TOKENS
  | STRINGLITERAL
  | NUMBERLITERAL
  | RULE
  | RULES
  | END
  | ANY
  | ALL
  | EXISTS
  | MAP
  | TRUE
  | FALSE
  | START
  | STOP
  | AT
  | IDENTIFIER
  | OPENBRACKET
  | CLOSEBRACKET
  | OPENBRACE
  | CLOSEBRACE
  | OPENPARENTHESIS
  | CLOSEPARENTHESIS
  | DOT
  | COMMA
  | EQUAL
  | SEMICOLON
  | COLON
  | EVENTARROW
  | GROUPARROW
  | BAR
  | NOTEQUAL
  | LESSTHAN
  | LESSTHANOREQUAL
  | GREATERTHAN
  | GREATERTHANOREQUAL
  | NEG
  | OR
  | AND
  | ADDITION
  | SUBTRACTION
  | MULTIPLICATION
  | DIVISION
  | ASSIGN
  | DEVICE
  | INPUT
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKENS, "$"),
  
    (STRINGLITERAL, "string_literal"),
    (NUMBERLITERAL, "number_literal"),
  
    (OPENBRACKET, "["),
    (CLOSEBRACKET, "]"),
    (OPENBRACE, "{"),
    (CLOSEBRACE, "}"),
    (OPENPARENTHESIS, "("),
    (CLOSEPARENTHESIS, ")"),
    
    (DOT, "."),
    (COMMA, ","),
    (SEMICOLON, ";"),
    (COLON, ":"),
    (EVENTARROW, "~>"),
    (GROUPARROW, "->"),
    (BAR, "|"),

    (OR, "||"),
    (AND, "&&"),
    (EQUAL, "=="),
    (NOTEQUAL, "!="),
    (LESSTHAN, "<"),
    (LESSTHANOREQUAL, "<="),
    (GREATERTHAN, ">"),
    (GREATERTHANOREQUAL, ">="),

    (NEG, "~"),
    (ADDITION, "+"),
    (SUBTRACTION, "-"),
    (MULTIPLICATION, "*"),
    (DIVISION, "/"),
    
    (ASSIGN, ":=")
  ] ++ keywords

keywords :: [(Token, String)]
keywords =
  [
    (RULE, "rule"),
    (RULES, "rules"),
    (END, "end"),
    (ANY, "any"),
    (ALL, "all"),
    (EXISTS, "exists"),
    (MAP, "map"),
    (TRUE, "true"),
    (FALSE, "false"),
    (START, "start"),
    (STOP, "stop"),
    (AT, "at"),
    (IDENTIFIER, "identifier"),
    (DEVICE, "device"),
    (INPUT, "input")
  ]

findTok :: Token -> [(Token, String)] -> Maybe String
findTok _ [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
      
  isEOT END_OF_TOKENS = True
  isEOT _             = False
    
