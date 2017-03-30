{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.State (
    EnvS,
    getEnvS,
    module Control.Monad.State
) where

import           Control.Monad.State

data EnvS = EnvS deriving (Show)

getEnvS :: IO EnvS
getEnvS = return EnvS

type MonadEnvState m = MonadState EnvS m
