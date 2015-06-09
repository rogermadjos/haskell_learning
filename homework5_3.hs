{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

class HasVars a where
  var :: String -> a

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

data VarExprT = Lit Integer
  | Var String
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

ops :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
ops f Nothing _ = Nothing
ops f _ Nothing = Nothing
ops f (Just a) (Just b) = Just (f a b)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a m = Just a
  add f1 f2 m = ops (+) (f1 m) (f2 m)
  mul f1 f2 m = ops (*) (f1 m) (f2 m)

withVars :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
