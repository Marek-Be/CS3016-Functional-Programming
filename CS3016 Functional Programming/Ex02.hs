{- betkam Marek Betka -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d
ins k d Nil = Leaf k d -- empty then make

ins k1 d1 (Leaf k2 d2)
    | k1 == k2 = Leaf k1 d1
    | k1 < k2  = Br (Leaf k1 d1) Nil k2 d2 -- new left
    | k1 > k2  = Br Nil (Leaf k1 d1) k2 d2 -- new right

ins k1 d1 (Br left right k2 d2)
    | k1 == k2 = Br left right k1 d1 
    | k1 < k2  = Br (ins k1 d1 left) right k2 d2 -- left
    | k1 > k2  = Br left (ins k1 d1 right) k2 d2 -- right

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp Nil _ = fail "Empty leaf" -- Empty

lkp (Leaf k d) lkpKey
    | lkpKey == k = return d 
    | otherwise   = fail "Key does not match with leaf" -- Fail lookup.

lkp (Br left right k d) lkpKey
    | lkpKey == k = return d       
    | lkpKey < k  = lkp left lkpKey  
    | lkpKey > k  = lkp right lkpKey 

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}


instance Num Expr where
  Val e1 + Val e2 = Val (e1 + e2)
  e1 + e2 = Add e1 e2
  Val e1 - Val e2 = Val (e1 - e2)
  e1 - e2 = Sub e1 e2
  Val e1 * Val e2 = Val (e1 * e2)
  e1 * e2 = Mul e1 e2
  negate e = (0 - e)
  abs e = Val 1.0
  signum 0 = Val 0.0
  signum e = Val 1.0
  fromInteger i = Val (fromIntegral(i))
