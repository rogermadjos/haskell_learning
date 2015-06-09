import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Nothing -> Nothing
  Just x -> Just $ eval x

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add a b = let f (MinMax x) = x in MinMax $ max (f a) (f b)
  mul a b = let f (MinMax x) = x in MinMax $ min (f a) (f b)

instance Expr Mod7 where
  lit = Mod7
  add a b = let f (Mod7 x) = x in Mod7 $ mod (f a + f b) 7
  mul a b = let f (Mod7 x) = x in Mod7 $ mod (f a * f b) 7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
