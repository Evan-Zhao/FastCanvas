{-# LANGUAGE DeriveDataTypeable #-}

module Settings.Exception.GeneralException (
    module Settings.Exception.GeneralException,
    SomeException
) where

import           Control.Exception
import           Data.Maybe                  (fromMaybe)
import           Data.Typeable
import           Settings.Exception.Prettify

newtype GeneralException = GeneralException { unGeneralException :: String }
                                deriving (Eq, Show, Typeable)

instance Exception GeneralException

fromString :: String -> SomeException
fromString = toException . GeneralException

toString :: SomeException -> String
toString e = fromMaybe (show e) prettified where
    prettified = findFirstJust $ map ($ e) dispatches
