-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module Parser where

import Scanner
import ParserCombis
import AbsSyn

type ParserT = Parser Token

tokenP :: Terminal -> ParserT Token
tokenP term = P (\inp ->
  case parse itemP inp of
    [] -> []
    [((term', attrib), out)] ->
      if term' == term then
        [((term', attrib), out)]
      else
        [])

tP :: Terminal -> ParserT ()
tP term = do tokenP term; return ()

aLitP :: ParserT Int
aLitP = do (ALITERAL, Just (ALitAttrib val)) <- tokenP ALITERAL; return val

bLitP :: ParserT Bool
bLitP = do (BLITERAL, Just (BLitAttrib val)) <- tokenP BLITERAL; return val

identP :: ParserT Ident
identP = do (IDENT, Just (IdentAttrib ident)) <- tokenP IDENT; return ident

arithOprP :: ParserT ArithOperator
arithOprP = do (ARITHOPR, Just (AOprAttrib opr)) <- tokenP ARITHOPR; return opr

relOprP :: ParserT RelOperator
relOprP = do (RELOPR, Just (ROprAttrib opr)) <- tokenP RELOPR; return opr

boolOprP :: ParserT BoolOperator
boolOprP = do (BOOLOPR, Just (BOprAttrib opr)) <- tokenP BOOLOPR; return opr

arithExprP :: ParserT ArithExpr
arithExprP =
      litAExprP
  +++ idAExprP
  +++ dyaAExprP

litAExprP = do val <- aLitP; return (LitAExpr val)

idAExprP = do ident <- identP; return (IdAExpr ident)

dyaAExprP =
  do (tP LPAREN)
     aExpr1 <- arithExprP
     aOpr   <- arithOprP
     aExpr2 <- arithExprP
     (tP RPAREN)
     return (DyaAExpr aOpr aExpr1 aExpr2)

boolExprP :: ParserT BoolExpr
boolExprP =
      litBExprP
  +++ relBExprP
  +++ negBExprP
  +++ dyaBExprP

litBExprP = do val <- bLitP; return (LitBExpr val)

relBExprP =
  do (tP LPAREN)
     aExpr1 <- arithExprP
     rOpr   <- relOprP
     aExpr2 <- arithExprP
     (tP RPAREN)
     return (RelBExpr rOpr aExpr1 aExpr2)

negBExprP =
  do (tP NOT)
     bExpr <- boolExprP
     return (NegBExpr bExpr)

dyaBExprP =
  do (tP LPAREN)
     bExpr1 <- boolExprP
     bOpr   <- boolOprP
     bExpr2 <- boolExprP
     (tP RPAREN)
     return (DyaBExpr bOpr bExpr1 bExpr2)

assertionP = boolExprP

commandP :: ParserT Command
commandP =
      skipCmdP
  +++ assiCmdP
  +++ condCmdP
  +++ whileCmdP

skipCmdP = do (tP SKIP); return SkipCmd

assiCmdP =
  do ident <- identP
     (tP BECOMES)
     aExpr <- arithExprP
     return (AssiCmd ident aExpr)

condCmdP =
  do (tP IF)
     bExpr <- boolExprP
     (tP THEN)
     cmd1  <- cpsCmdP
     (tP ELSE)
     cmd2  <- cpsCmdP
     (tP ENDIF)
     return (CondCmd bExpr cmd1 cmd2)

whileCmdP =
  do (tP WHILE)
     bExpr <- boolExprP
     (tP INVARIANT)
     invar <- assertionP
     (tP DO)
     cmd   <- cpsCmdP
     (tP ENDWHILE)
     return (WhileCmd bExpr invar cmd)

cpsCmdP :: ParserT Command
cpsCmdP = sepList1C commandP (tP SEMICOLON) CpsCmd

programP :: ParserT Program
programP =
  do (tP PRECONDITION)
     precond <- assertionP
     (tP POSTCONDITION)
     postcond <- assertionP
     (tP COMMAND)
     cmd <- cpsCmdP
     (tP SENTINEL)
     return (Program precond postcond cmd)

parser :: [Token] -> Program
parser toks =
  case parse programP toks of
    [(prog, [])] -> prog
    [_]          -> error "internal error"
    []           -> error "syntax error"
