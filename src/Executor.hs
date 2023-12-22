{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Executor where

import CustomDataTypes
import Control.Applicative ((<|>), empty, Alternative(many))

newtype Executor a = Executor {
  runExecutor :: ParsedStatement -> Either ErrorMessage a
}

instance Functor Executor where
  fmap :: (a -> b) -> Executor a -> Executor b
  fmap f functor = Executor $ \st ->
    case runExecutor functor st of
      Left l -> Left l
      Right r -> Right $ f r

instance Applicative Executor where 
  pure :: a -> Executor a
  pure a = Executor $ \_ -> Right a
  (<*>) :: Executor (a -> b) -> Executor a -> Executor b
  fl <*> fr = Executor $ \st ->
    case runExecutor fl st of
      Left l1 -> Left l1
      Right r1 -> case runExecutor fr st of
        Left l2 -> Left l2
        Right r2 -> Right (r1 r2)

instance Alternative Executor where
  empty :: Executor a
  empty = Executor $ \_-> Left "Error"
  (<|>) :: Executor a -> Executor a -> Executor a
  e1 <|> e2 = Executor $ \st ->
    case runExecutor e1 st of 
      Right r1 -> Right r1 
      Left _ -> case runExecutor e2 st of
        Right r2 -> Right r2
        Left l2 -> Left l2

instance Monad Executor where 
  (>>=) :: Executor a -> (a -> Executor b) -> Executor b
  e >>= f = Executor $ \st ->
    case runExecutor e st of
      Left l1 -> Left l1
      Right r1 -> case runExecutor (f r1) st of 
        Left l2 -> Left l2
        Right r2 -> Right r2