module Except (
    module Except,
    lift,
    runExceptT,
    throwE
) where

import           Control.Exception
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except

type MainExcept = ExceptT String IO

showException :: SomeException -> ExceptT String IO a
showException = throwE . show

catchMainE :: IO a -> MainExcept a
catchMainE action = catchE (ExceptT $ try action) showException

eitherToE :: Either String a -> MainExcept a
eitherToE = ExceptT . return

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x)  = Right x
maybeToEither str Nothing = Left str

maybeToE :: String -> Maybe a -> MainExcept a
maybeToE str = eitherToE . maybeToEither str

try' :: IO a -> IO (Either SomeException a)
try' = try
