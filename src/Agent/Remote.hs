{-# LANGUAGE TemplateHaskell #-}
module Agent.Remote (
    agent
  , __remoteTable
  , agent__static
  , agent__sdict
  , agent__tdict
  ) where

import           Agent.Data.Log (Log (..))
import qualified Agent.Data.Log as Log
import           Agent.Protocol

import           Control.Monad (unless)
import           Control.Distributed.Process (Process, ProcessId)
import qualified Control.Distributed.Process as Process
import           Control.Distributed.Process.Closure (remotable)

import           Data.Foldable (for_)

import           Prelude hiding (log)


-- |
-- Replicate @log@ to every @peer@.
_replication :: [ProcessId] -> Log ProcessId -> Process (Log ProcessId)
_replication peers log = do
  for_ peers $ \pid -> do
    let d = Log.delta pid log
    unless (Log.null d) $ do
      Process.reconnect pid
      Process.send pid (Replicate d)
  pure $ log

agent :: ProcessId -> Process ()
agent client =
  Process.expect >>= \message -> case message of
    Initialise _ _ _ _ -> do
      self <- Process.getSelfPid
      Process.say "it is alive!"
      Process.send client (Complete self 0 0)

remotable ['agent]
