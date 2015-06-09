{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import StackVM
import Parser

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
