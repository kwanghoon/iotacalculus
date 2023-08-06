module Token (Token(..)) where

import TokenInterface

data Token =
    IDENTIFIER
  | END_OF_TOKENS
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKENS, "$"),
  
    (STRINGLITERAL, "string_literal"),
    (NUMBERLITERAL, "number_literal"),
  
    (RULE, "rule"),
    (RULES, "rules"),
    (END, "end"),
    (ALL, "all"),
    (EXISTS, "exists"),
    (MAP, "map"),
    (TRUE, "true"),
    (FALSE, "false"),
    (START, "start"),
    (STOP, "stop"),
    (AT, "at"),
    (DEVICES, "devices"),
    (IDENTIFIER, "identifier"),
    
    (OPENBRACKET, "["),
    (CLOSEBRACKET, "]"),
    (OPENBRACE, "{"),
    (CLOSEBRACE, "}"),
    (OPENPARENTHESIS, "("),
    (CLOSEPARENTHESIS, ")"),
    
    (DOT, "."),
    (COMMA, ","),
    (SEMICOLON, ";"),
    (EVENTARROW, "~>"),
    (GROUPARROW, "->"),
    
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
  ]

findTok tok [] = Nothing
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
    
