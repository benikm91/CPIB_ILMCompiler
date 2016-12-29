-- IML, HS-2012, Ruedi
-- Edgar F.A. Lederer, FHNW

module BaseDecls where

type BaseIdent = String

data Operator
  = Not
  | Times
  | DivE -- euclidean
  | ModE
  | DivF -- floored
  | ModF
  | DivT -- truncated
  | ModT
  | Plus
  | Minus
  | Less
  | GreaterEq
  | Equal
  | NotEq
  | Greater
  | LessEq
  | CondAnd
  | CondOr
  deriving (Eq, Show)

isDivOpr :: Operator -> Bool
isDivOpr DivE = True
isDivOpr ModE = True
isDivOpr DivF = True
isDivOpr ModF = True
isDivOpr DivT = True
isDivOpr ModT = True
isDivOpr _    = False

isArithOpr :: Operator -> Bool
isArithOpr Plus  = True
isArithOpr Minus = True
isArithOpr Times = True
isArithOpr opr   = isDivOpr opr

isOrderingOpr :: Operator -> Bool
isOrderingOpr Less      = True
isOrderingOpr GreaterEq = True
isOrderingOpr Greater   = True
isOrderingOpr LessEq    = True
isOrderingOpr _         = False

isEqualityOpr :: Operator -> Bool
isEqualityOpr Equal   = True
isEqualityOpr Greater = True
isEqualityOpr _       = False

isBoolOpr :: Operator -> Bool
isBoolOpr CondAnd = True
isBoolOpr CondOr  = True
isBoolOpr _       = False

data Type
  = BoolTy
  | IntTy WordLength
  deriving (Eq, Show)

type WordLength = Int

data FlowMode
  = InFlow
  | InoutFlow
  | OutFlow
  deriving (Eq, Show)

data MechMode
  = CopyMech
  | RefMech
  deriving (Eq, Show)

data ChangeMode
  = Const
  | Var
  deriving (Eq, Show)

data Value
  = BoolVal Bool
  | IntegerVal Integer
  deriving (Eq, Show)

isOutFlow, isReadFlow, isInFlow, isWriteFlow :: FlowMode -> Bool
isOutFlow OutFlow = True
isOutFlow _       = False

isReadFlow = not . isOutFlow -- readFlow is thus InFlow or InoutFlow

isInFlow InFlow = True
isInFlow _      = False

isWriteFlow = not . isInFlow -- writeFlow is thus InoutFlow or OutFlow

isOutgoingCopy :: FlowMode -> MechMode -> Bool
isOutgoingCopy InoutFlow CopyMech = True
isOutgoingCopy OutFlow   CopyMech = True
isOutgoingCopy _         _        = False
