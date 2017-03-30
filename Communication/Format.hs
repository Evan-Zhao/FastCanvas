{-# LANGUAGE DeriveGeneric #-}

module Communication.Format where

import qualified Course.List           as CL
import           Data.Aeson
import           Files.State
import           GHC.Generics
import           Settings.Monad.Global

newtype ApiResponse = ApiResponse {
    unResponse :: Either SomeException [(CL.Course, DownloadState)]
} deriving (Generic)

instance ToJSON ApiResponse
