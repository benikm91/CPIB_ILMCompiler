-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module AbsSyn where

type Ident = String

data ArithOperator
  = Power
  | Times
  | Div
  | Mod
  | Plus
  | Minus
  deriving (Eq, Show)

data RelOperator
  = Less
  | GreaterEq
  | Equal
  | NotEq
  | Greater
  | LessEq
  deriving (Eq, Show)

data BoolOperator
  = CondAnd
  | CondOr
  | Implies
  deriving (Eq, Show)

data ArithExpr
  = LitAExpr Int
  | IdAExpr Ident
  | DyaAExpr ArithOperator ArithExpr ArithExpr
  deriving (Eq, Show)

data BoolExpr
  = LitBExpr Bool
  | RelBExpr RelOperator ArithExpr ArithExpr
  | NegBExpr BoolExpr
  | DyaBExpr BoolOperator BoolExpr BoolExpr
  deriving (Eq, Show)

type Assertion = BoolExpr

data Command
  = SkipCmd
  | AssiCmd Ident ArithExpr
  | CpsCmd [Command]
  | CondCmd BoolExpr Command Command
  | WhileCmd BoolExpr Assertion Command
  deriving (Eq, Show)

data Program
  = Program Assertion Assertion Command
  deriving (Eq, Show)

ppAOpr :: ArithOperator -> String
ppAOpr Power = "^"
ppAOpr Times = "*"
ppAOpr Div   = " div "
ppAOpr Mod   = " mod "
ppAOpr Plus  = "+"
ppAOpr Minus = "-"

ppROpr :: RelOperator -> String
ppROpr Less      = "<"
ppROpr GreaterEq = ">="
ppROpr Equal     = "="
ppROpr NotEq     = "/="
ppROpr Greater   = ">"
ppROpr LessEq    = "<="

ppBOpr :: BoolOperator -> String
ppBOpr CondAnd = "&"
ppBOpr CondOr  = "|"
ppBOpr Implies = " -> "

ppAExpr :: ArithExpr -> String
ppAExpr (LitAExpr c) = show c
ppAExpr (IdAExpr ident) = ident
ppAExpr (DyaAExpr opr expr1 expr2) = "(" ++ s1 ++ sOpr ++ s2 ++ ")"
  where
    sOpr = ppAOpr opr
    s1 = ppAExpr expr1
    s2 = ppAExpr expr2

ppBExpr :: BoolExpr -> String
ppBExpr (LitBExpr b) = show b
ppBExpr (RelBExpr opr expr1 expr2) = "(" ++ s1 ++ sOpr ++ s2 ++ ")"
  where
    sOpr = ppROpr opr
    s1 = ppAExpr expr1
    s2 = ppAExpr expr2
ppBExpr (NegBExpr expr) = "not " ++ ppBExpr expr
ppBExpr (DyaBExpr opr expr1 expr2) = "(" ++ s1 ++ sOpr ++ s2 ++ ")"
  where
    sOpr = ppBOpr opr
    s1 = ppBExpr expr1
    s2 = ppBExpr expr2
