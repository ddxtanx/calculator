module Failable
(
    Failable(Result, Error)
) where

data Failable a = Result a | Error String
    deriving (Show, Eq)

failed :: Failable a -> Bool
failed (Result _) = False
failed _ = True

instance Functor Failable where
    fmap f x = case x of
        Error str -> Error str
        Result x' -> Result $ f x'

instance Applicative Failable where
    pure = Result

    (Error e1) <*> (Error e2) = Error (e1 ++ "\n" ++ e2)
    (Error e1) <*> _ = Error e1
    _ <*> (Error e2) = Error e2
    (Result f) <*> (Result x) = Result $ f x

instance Monad Failable where
    return = Result
    (>>=) x f = case x of
        Error str -> Error str
        Result x' -> f x'
