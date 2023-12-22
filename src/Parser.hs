{-# LANGUAGE InstanceSigs #-}
module Parser (Parser(..)) where

import CustomDataTypes
import Control.Applicative ((<|>), empty, Alternative())

newtype Parser a = Parser {
  runParser :: String -> Either ErrorMessage (String, a)
}
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f functor = Parser $ \inp ->
    case runParser functor inp of
      Left l -> Left l
      Right (in1, r) -> Right (in1, f r)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \inp -> Right (inp, a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fl <*> fr = Parser $ \in1 ->
    case runParser fl in1 of
      Left l1 -> Left l1
      Right (in2, r1) -> case runParser fr in2 of
        Left e2 -> Left e2
        Right (in3, r2) -> Right (in3, r1 r2)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "Error"
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \inp ->
    case runParser p1 inp of
        Right r1 -> Right r1
        Left _ -> case runParser p2 inp of
            Right r2 -> Right r2
            Left e -> Left e

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \in1 ->
    case runParser p in1 of
      Left e1 -> Left e1
      Right (in2, r1) -> case runParser (f r1) in2 of
        Left e2 -> Left e2
        Right (in3, r2) -> Right (in3, r2)