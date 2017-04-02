{-# LANGUAGE DeriveDataTypeable #-}

module Settings.Exception.GeneralException (
    module Settings.Exception.GeneralException,
    SomeException
) where

import           Control.Exception
import           Data.Bifunctor              (first)
import           Data.Maybe                  (fromMaybe)
import           Data.Typeable
import           Settings.Exception.Prettify

newtype GeneralException = GeneralException { unGeneralException :: String }
                                deriving (Eq, Show, Typeable)

instance Exception GeneralException

fromString :: String -> SomeException
fromString = toException . GeneralException

toString :: SomeException -> String
toString = showException

leftToString :: Either SomeException a -> Either String a
leftToString = first toString
