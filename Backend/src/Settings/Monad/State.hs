{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.State (
    EnvS,
    getEnvS,
    MonadEnvState,
    module Control.Monad.State,
    module Control.Concurrent.Chan,
    Value
) where

import           Control.Concurrent.Chan
import           Control.Monad.State
import           Data.Aeson

type EnvS = Chan Value

getEnvS :: IO EnvS
getEnvS = newChan

type MonadEnvState m = MonadState EnvS m
