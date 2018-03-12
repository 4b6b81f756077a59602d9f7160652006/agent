import           Control.Monad (when, sequence)

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Agent.Data.Clock
import qualified Test.Agent.Data.Random
import qualified Test.Agent.Data.Timer
import qualified Test.Agent.Data.VectorClock


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> sequence [
      Test.Agent.Data.Clock.tests
    , Test.Agent.Data.Random.tests
    , Test.Agent.Data.Timer.tests
    , Test.Agent.Data.VectorClock.tests
    ] >>= \rs -> when (not . and $ rs) exitFailure
