module Settings.Global (
    module Settings.Global,
    lift,
    runExceptT,
    throwE,
    ExceptT
) where

import           Control.Exception
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Settings.Exception

type Global = ExceptT SomeException IO

catchE' :: IO a -> Global a
catchE' action = catchE (ExceptT $ try action) throwE

eitherToE :: Either String a -> Global a
eitherToE = ExceptT . return . mapLeft fromString where
    mapLeft f (Left x)  = Left (f x)
    mapLeft _ (Right r) = Right r

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither _ (Just x) = Right x
maybeToEither s Nothing  = Left s

maybeToE :: String -> Maybe a -> Global a
maybeToE str = eitherToE . maybeToEither str

try' :: IO a -> IO (Either SomeException a)
try' = try
