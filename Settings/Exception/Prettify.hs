module Settings.Exception.Prettify (
    showException
) where

import           Control.Exception
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe)

import           Settings.Exception.Prettify.HttpException

dispatches :: [SomeException -> Maybe String]
dispatches = [pHttpException]

findFirstJust :: [Maybe a] -> Maybe a
findFirstJust maybes = if null cat then Nothing else Just $ head cat where
    cat = catMaybes maybes

showException :: SomeException -> String
showException ex = fromMaybe (show ex) maybePrettified where
    maybePrettified = findFirstJust $ map ($ ex) dispatches
