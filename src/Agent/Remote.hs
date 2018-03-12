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

-- |
-- Receive @log@ from a @peer@. This is non blocking. The @Left@ output
-- is used to indicate that no message was received, the @Right@ output
-- is used to indicate that a messasge was received. They contain the
-- same log and can be unified with @either id id@, but keeping track
-- of the distinction allows downstream to decide how to handle the
-- different paths.
_receive :: Log ProcessId -> Process (Either (Log ProcessId) (Log ProcessId))
_receive log =
  Process.expectTimeout 0 >>= \message -> case message of
    Just (Replicate remote) -> do
      self <- Process.getSelfPid
      pure . Right $ Log.merge self log remote
    Nothing ->
      pure . Left $ log


agent :: ProcessId -> Process ()
agent client =
  Process.expect >>= \message -> case message of
    Initialise _ _ _ _ -> do
      self <- Process.getSelfPid
      Process.say "it is alive!"
      Process.send client (Complete self 0 0)

remotable ['agent]
