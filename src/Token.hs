module Token where

type Token = (Terminal, Maybe Attribute)
data Terminal 
  =
  -- brackets
  LBRACKET
  | RBRACKET
  -- signs
  | NOT
  | SEMICOLON 
  | RELOPR
  | BOOLOPR
  -- Keywords
  | WHILE
  | IF
  | THEN
  | ELSE
  -- Others
  | SKIP
  | BECOMES
  | SENTINEL
  | LITERAL
  | IDENT deriving (Show)

data Attribute
  = IntLitAttr Int
  | BoolLitAttr Bool
  | IdentAttr String
  | RelOprAttr RelOprType
  | BracketAttr BracketType
  | BoolOprAttr BoolOprType deriving (Show)

data RelOprType = Less | LessEq | Greater | GreaterEq | Eq  deriving (Show)
data BoolOprType = And | Or | CAnd | COr deriving(Show)
data BracketType = Round | Curly | Square deriving (Show)
