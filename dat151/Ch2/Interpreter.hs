module Interpreter where

import AbsCalc

eval :: Exp -> Integer
eval (EAdd e1 e2) = eval e1 + eval e2
eval (ESub e1 e2) = eval e1 - eval e2
eval (EMul e1 e2) = eval e1 * eval e2
eval (EDiv e1 e2) = div (eval e1) (eval e2)
eval (EInt n)     = n
