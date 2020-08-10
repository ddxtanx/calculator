module AbList
(
    AbList(AbNil, (:/))
) where

data AbList a b = AbNil | (:/) a (AbList b a) 
infixr 5 :/
