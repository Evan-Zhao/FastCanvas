{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Endpoint.Courses (
    thisTermCourse,
    courseShortName,
    Course (..),
    Courses
) where

import           Data.Aeson
import           GHC.Generics
import           Text.Parsec

import           Settings.Network

data Course = Course {
    id                 :: Int,
    name               :: String,
    enrollment_term_id :: Int
} deriving (Eq, Show, Generic)
instance ToJSON Course
instance FromJSON Course

type Courses = [Course]

takeThisTermCourse :: [Course] -> [Course]
takeThisTermCourse = filter ((== thisTermId) . enrollment_term_id)
  where
    thisTermId = 86490000000000005

thisTermCourse :: RIOE' m => m [Course]
thisTermCourse = do
    respJSON <- canvasJSON "courses"
    return $ takeThisTermCourse respJSON

courseShortName :: Course -> String
courseShortName c = either (const "") Prelude.id $ parse shortNameP "" $ name c

shortNameP = do
    a <- sequence [oneOf "VTP", oneOf ['A'..'Z']]
    b <- count 3 digit
    return $ a ++ b
