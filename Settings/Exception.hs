{-# LANGUAGE DeriveDataTypeable #-}

module Settings.Exception (
    module Settings.Exception,
    SomeException
) where

import           Control.Exception
import           Data.Maybe        (catMaybes, fromMaybe)
import           Data.Typeable

newtype GeneralException = GeneralException { unGeneralException :: String }
                                deriving (Eq, Show, Typeable)

instance Exception GeneralException

fromString :: String -> SomeException
fromString = toException . GeneralException

toString :: SomeException -> String
toString e = fromMaybe (show e) prettified where
    prettified = findFirstJust $ map ($ e) dispatches

dispatches :: [SomeException -> Maybe String]
dispatches = []

findFirstJust :: [Maybe a] -> Maybe a
findFirstJust maybes = if null cat then Nothing else Just $ head cat where
    cat = catMaybes maybes
