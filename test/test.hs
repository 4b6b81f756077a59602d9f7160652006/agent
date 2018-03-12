import           Control.Monad (when, sequence)

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Agent.Data.Random
import qualified Test.Agent.Data.Timer


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> sequence [
      Test.Agent.Data.Random.tests
    , Test.Agent.Data.Timer.tests
    ] >>= \rs -> when (not . and $ rs) exitFailure
