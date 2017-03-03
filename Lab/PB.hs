import           Control.Concurrent           (threadDelay)
import           Control.Monad                (unless)
import           System.Console.AsciiProgress (Options (..), def,
                                               displayConsoleRegions,
                                               isComplete, newProgressBar, tick)

main :: IO ()
main = displayConsoleRegions $ do
    pg <- newProgressBar def { pgWidth = 100
                             , pgCompletedChar = '='
                             , pgFormat = "Downloading :percent [:bar]\
                              \ :current/:total (:eta s remaining)"
                             }
    loop pg
  where
    loop pg = do
        b <- isComplete pg
        unless b $ do
            threadDelay $ 200 * 1000
            tick pg
            loop pg
