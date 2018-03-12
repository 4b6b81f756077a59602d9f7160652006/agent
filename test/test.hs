import           Control.Monad (when, sequence)

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> sequence [
    ] >>= \rs -> when (not . and $ rs) exitFailure
