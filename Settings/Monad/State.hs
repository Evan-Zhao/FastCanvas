{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.State (
    EnvS,
    getEnvS,
    MonadEnvState,
    module Control.Monad.State
) where

import           Control.Concurrent.Chan
import           Control.Monad.State

type EnvS a = Chan a

getEnvS :: IO (EnvS a)
getEnvS = newChan

type MonadEnvState a m = MonadState (EnvS a) m
