import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import qualified Data.Traversable         as T

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

-- A little test:
main = mapPool 10 (\x -> threadDelay 1000000 >> print x) [1..100]
