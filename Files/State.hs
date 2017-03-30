{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Files.State where

import           Control.Exception
import           Data.Aeson
import           Data.Monoid       ((<>))
import           GHC.Generics

data DownloadResult = DownloadResult {
    filePath       :: String,
    downloadResult :: ResultEnum
}

data ResultEnum = Succeed | Exists | Failed SomeException

instance Show ResultEnum where
    show Succeed     = "was successfully written."
    show Exists      = "was already there."
    show (Failed ex) = "failed to write with exception " ++ show ex

instance Show DownloadResult where
    show DownloadResult{..} =
        "Entry " ++ filePath ++ " " ++ show downloadResult

data DownloadSummary = DownloadSummary {
    existN        :: Int,
    succeedN      :: Int,
    failedN       :: Int,
    failureSample :: [SomeException]
}

instance ToJSON SomeException where
    toJSON e = toJSON $ show e
    toEncoding e = toEncoding $ show e

instance ToJSON DownloadSummary where
    toJSON DownloadSummary {..} =
        object ["exist" .= existN, "succeed" .= succeedN
              , "failed" .= failedN
              , "exception" .= failureSample
            ]

    toEncoding DownloadSummary {..} =
        pairs ("exist" .= existN <> "succeed" .= succeedN
            <> "failed" .= failedN
            <> "exception" .= failureSample
            )

instance Show DownloadSummary where
    show (DownloadSummary 0 0 0 _) = "No files to download."
    show (DownloadSummary _ 0 0 _) = "All file exists; no change applied."
    show (DownloadSummary 0 _ 0 _) = "Successfully downloaded all files."
    show (DownloadSummary 0 0 _ _) = "All downloads failed."
    show (DownloadSummary e s f r) = succ' ++ fail' ++ exist where
        succ' = "Successfully downloaded " ++ show s ++ " files."
        fail' = if f /= 0 then "\n" ++ show f ++
                               " files failed when downloading; faliure reasons include" ++ show r
                          else ""
        exist = if e /= 0 then "\n" ++ show e ++ " files were already there; no change applied." else ""

instance Monoid DownloadSummary where
    mempty = DownloadSummary 0 0 0 []
    mappend state1 state2 = DownloadSummary {
        existN = existN state1 + existN state2,
        succeedN = succeedN state1 + succeedN state2,
        failedN = failedN state1 + failedN state2,
        failureSample = failureSample state2 ++ failureSample state1
    }

isAllFail :: DownloadSummary -> Bool
isAllFail (DownloadSummary 0 0 f _) = f /= 0
isAllFail _                         = False

singleExState, singleSuState :: DownloadSummary
singleExState = DownloadSummary 1 0 0 []
singleSuState = DownloadSummary 0 1 0 []

singleFaStateWith :: SomeException -> DownloadSummary
singleFaStateWith = DownloadSummary 0 0 1 . (:[])
