{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Files.State where

import           Control.Exception
import           Data.Aeson
import           Data.Monoid       ((<>))
import           GHC.Generics

data DownloadState = DownloadState {
    existN        :: Int,
    succeedN      :: Int,
    failedN       :: Int,
    failureSample :: [SomeException]
}

instance ToJSON SomeException where
    toJSON e = toJSON $ show e
    toEncoding e = toEncoding $ show e

instance ToJSON DownloadState where
    toJSON DownloadState {..} =
        object ["exist" .= existN, "succeed" .= succeedN
              , "failed" .= failedN
              , "exception" .= failureSample
            ]

    toEncoding DownloadState {..} =
        pairs ("exist" .= existN <> "succeed" .= succeedN
            <> "failed" .= failedN
            <> "exception" .= failureSample
            )

instance Show DownloadState where
    show (DownloadState 0 0 0 _) = "No files to download."
    show (DownloadState _ 0 0 _) = "All file exists; no change applied."
    show (DownloadState 0 _ 0 _) = "Successfully downloaded all files."
    show (DownloadState 0 0 _ _) = "All downloads failed."
    show (DownloadState e s f r) = succ' ++ fail' ++ exist where
        succ' = "Successfully downloaded " ++ show s ++ " files."
        fail' = if f /= 0 then "\n" ++ show f ++
                               " files failed when downloading; faliure reasons include" ++ show r
                          else ""
        exist = if e /= 0 then "\n" ++ show e ++ " files were already there; no change applied." else ""

instance Monoid DownloadState where
    mempty = DownloadState 0 0 0 []
    mappend state1 state2 = DownloadState {
        existN = existN state1 + existN state2,
        succeedN = succeedN state1 + succeedN state2,
        failedN = failedN state1 + failedN state2,
        failureSample = failureSample state2 ++ failureSample state1
    }

isAllFail :: DownloadState -> Bool
isAllFail (DownloadState 0 0 f _) = f /= 0
isAllFail _                       = False

singleExState, singleSuState :: DownloadState
singleExState = DownloadState 1 0 0 []
singleSuState = DownloadState 0 1 0 []

singleFaStateWith :: SomeException -> DownloadState
singleFaStateWith = DownloadState 0 0 1 . (:[])
