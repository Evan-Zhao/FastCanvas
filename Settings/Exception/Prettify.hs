module Settings.Exception.Prettify where

import           Control.Exception
import           Data.Maybe        (catMaybes)

dispatches :: [SomeException -> Maybe String]
dispatches = []

findFirstJust :: [Maybe a] -> Maybe a
findFirstJust maybes = if null cat then Nothing else Just $ head cat where
    cat = catMaybes maybes
