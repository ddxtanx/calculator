{-|
Module      : Failable
Description : A monad that helps manage failable computations.
Copyright   : (c) Garrett Credi (2020)
License     : GPL-3
Maintainer  : garrettcredi@gmail.com
Stability   : stable

This module provides the monad Failable which either maps and computes with
successfull results like normal, or keeps track of any errors that arose through
computations.
-}
module Failable
(
    Failable(..),
    failed
) where

-- | Failable is a monad representing a failable computation. If a computation 
-- | succeeds, then it is simply a Result (result), if it fails it is an
-- | Error (error) that includes a string detailing what error ocurred.
data Failable a = 
    Result a -- ^ A successfull computation
    | Error String -- ^ A computation that resulted in an error.
    deriving (Show, Eq)

-- | Determines whether a failable computation failed.
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
    return = pure
    (>>=) (Error str) f = Error str
    (>>=) (Result x) f = f x
