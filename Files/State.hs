{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Files.State where

import           Control.Exception
import           Data.Aeson
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import           GHC.Generics

instance ToJSON SomeException where
    toJSON e = toJSON $ show e
    toEncoding e = toEncoding $ show e

data DownloadResult = DownloadResult {
    filePath :: String,
    result   :: ResultEnum
} deriving (Generic)

instance ToJSON DownloadResult

data ResultEnum = Succeed | Exists | Failed SomeException deriving (Generic)

instance ToJSON ResultEnum where
    toJSON Succeed = object ["code" .= ("succeed" :: Text)]
    toJSON Exists = object ["code" .= ("exists" :: Text)]
    toJSON (Failed ex) = object [
        "code" .= ("failed" :: Text),
        "diagnose" .= toJSON ex ]

    toEncoding = genericToEncoding defaultOptions

instance Show ResultEnum where
    show Succeed     = "was successfully written."
    show Exists      = "was already there."
    show (Failed ex) = "failed to write with exception " ++ show ex

instance Show DownloadResult where
    show DownloadResult{..} =
        "Entry " ++ filePath ++ " " ++ show result

singleExState, singleSuState :: FilePath -> DownloadResult
singleExState path = DownloadResult path Exists
singleSuState path = DownloadResult path Succeed

singleFaStateWith :: FilePath -> SomeException -> DownloadResult
singleFaStateWith path ex = DownloadResult path (Failed ex)


data DownloadSummary = DownloadSummary {
    existN        :: Int,
    succeedN      :: Int,
    failedN       :: Int,
    failureSample :: [SomeException]
}

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

summarize :: DownloadResult -> DownloadSummary
summarize (DownloadResult path Succeed) = DownloadSummary 0 1 0 []
summarize (DownloadResult path Exists) = DownloadSummary 1 0 0 []
summarize (DownloadResult path (Failed reason)) = DownloadSummary 0 1 0 [reason]
