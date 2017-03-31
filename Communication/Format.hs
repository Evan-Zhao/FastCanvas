{-# LANGUAGE DeriveGeneric #-}

module Communication.Format(
    ApiResponse,
    unResponse,
    prettifyException
) where

import           Data.Bifunctor

import qualified Course.List                         as CL
import           Data.Aeson
import           Files.State
import           GHC.Generics
import           Settings.Exception.GeneralException
import           Settings.Monad.Global

newtype ApiResponse = ApiResponse {
    unResponse :: Either String [(CL.Course, DownloadSummary)]
} deriving (Generic)

instance ToJSON ApiResponse

prettifyException :: Either SomeException [(CL.Course, DownloadSummary)]
                  -> ApiResponse
prettifyException = ApiResponse . first toString
