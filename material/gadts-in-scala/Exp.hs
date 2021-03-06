{-# LANGUAGE GADTs, ScopedTypeVariables, RankNTypes, FlexibleContexts #-}
module Exp where

data Exp a where
  LitInt  :: Int                        -> Exp Int
  LitBool :: Bool                       -> Exp Bool
  Add     :: Exp Int -> Exp Int         -> Exp Int
  Mul     :: Exp Int -> Exp Int         -> Exp Int
  Cond    :: Exp Bool -> Exp a -> Exp a -> Exp a
  EqE     :: Eq a => Exp a -> Exp a     -> Exp Bool

eval :: Exp a -> a
eval e = case e of
  LitInt i       -> i
  LitBool b      -> b
  Add e e'       -> eval e + eval e'
  Mul e e'       -> eval e * eval e'
  Cond b thn els -> if eval b then eval thn else eval els
  EqE e e'       -> eval e == eval e'

test = eval $ Cond (EqE (Add (LitInt 1) (LitInt 2)) (LitInt 3))
              (Mul (LitInt 5) (LitInt 7)) (LitInt 13)

test2 = eval $ Cond (LitBool True) (LitInt 1) (LitInt 3)