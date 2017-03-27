import           Control.Concurrent.Async

main = mapConcurrently_ f [1..1000] where
    f x = putStrLn $ "Outputing " ++ show x
