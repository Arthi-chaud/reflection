{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Data.JSON.Parser.Any where

import Control.Applicative
import Data.Functor
import Data.JSON.Parser.Base (Parser, parseBool, parseDouble, parseNull, parseString)
import Data.JSON.TH (genFromJSON)

parseAny :: Parser ()
parseAny =
    parseNull $> ()
        <|> parseString $> ()
        <|> parseDouble $> ()
        <|> parseBool $> ()
        <|> parseAnyObject $> ()

data AnyObject = MkAO {}

genFromJSON ''AnyObject
