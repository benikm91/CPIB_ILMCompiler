module Token where

type Token = (Terminal, Maybe Attribute)
data Terminal 
  =
  -- brackets
  L_BRACKET_ROUND
  | R_BRACKET_ROUND
  -- signs
  | NOT
  | SEMICOLON 
  | RELOPR
  | BOOLOPR
  | COLON
  | COMMA
  | ASSIGNMENT
  -- Arithmetic
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  -- Keywords
  | DO
  | WHILE
  | END_WHILE
  | IF
  | THEN
  | ELSE
  | PROGRAM
  | END_PROGRAM
  | IN
  | OUT
  | CONST
  | TYPE
  | GLOBAL
  | PROCEDURE
  | END_PROCEDURE
  -- Others
  | SKIP
  | COPY
  | REF
  | BECOMES
  | SENTINEL
  | LITERAL
  | IDENT deriving (Show)

data Attribute
  = IntLitAttr Int
  | BoolLitAttr Bool
  | IdentAttr String
  | RelOprAttr RelOprType
  | BoolOprAttr BoolOprType 
  | TypeAttr TypeType deriving (Show)

data TypeType = Integer Int deriving (Show)
data RelOprType = Less | LessEq | Greater | GreaterEq | Eq deriving (Show)
data BoolOprType = And | Or | CAnd | COr deriving(Show)
