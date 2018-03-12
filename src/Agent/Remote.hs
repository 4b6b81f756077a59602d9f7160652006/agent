{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Agent.Remote (
    agent
  , __remoteTable
  , agent__static
  , agent__sdict
  , agent__tdict
  ) where

import           Agent.Data.Log (Log (..), Value (..))
import qualified Agent.Data.Log as Log
import           Agent.Data.Random (Random (..))
import qualified Agent.Data.Random as Random
import           Agent.Data.Timer (Timer (..))
import qualified Agent.Data.Timer as Timer
import           Agent.Protocol

import           Control.Distributed.Process (Process, ProcessId)
import qualified Control.Distributed.Process as Process
import           Control.Distributed.Process.Closure (remotable)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Foldable (for_)

import           Prelude hiding (log)


-- |
-- Replicate @log@ to every @peer@.
--
replication :: [ProcessId] -> Log ProcessId -> Process (Log ProcessId)
replication peers log = do
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
--
receive :: Log ProcessId -> Process (Either (Log ProcessId) (Log ProcessId))
receive log =
  Process.expectTimeout 0 >>= \message -> case message of
    Just (Replicate remote) -> do
      self <- Process.getSelfPid
      pure . Right $ Log.merge self log remote
    Nothing ->
      pure . Left $ log

-- |
-- Generate and append a new pseudo-random value to the log.
--
append :: Random -> Log ProcessId  -> Process (Log ProcessId)
append random log = do
  self <- Process.getSelfPid
  value <- Value <$> Random.next random
  pure $ Log.append self value log

-- |
-- Aggregate results, print and notify leader that we are done.
--
finalise :: ProcessId -> Log ProcessId -> Process ()
finalise leader log = do
  self <- Process.getSelfPid
  let
    !messages = Log.length log
    !total = Log.total log
  Process.say $ mconcat ["messages = ", show messages, ", total = ", show total]
  Process.reconnect leader
  Process.send leader (Complete self messages total)

data State =
    Sending !ProcessId !Random !(Log ProcessId) ![ProcessId] !Timer !Timer
  | Grace !ProcessId !(Log ProcessId) !Timer

-- |
-- The main loop. The loop operates in two phases @Sending@ or @Grace@.
-- During @Sending@ the loop is interleaving receives and sends, trying
-- to get messages to all peers. During @Grace@ we no longer send, but
-- do merge any replication events till on the wire. Each phase has a
-- fixed time limit. @Grace@ will finish early if there are no more
-- messages.
--
loop :: State -> Process ()
loop (Sending client random log peers sendUntil waitUntil) = do
  done <- Timer.expired sendUntil
  if not done then do
    updated <- receive log >>= append random . either id id >>= replication peers
    loop (Sending client random updated peers sendUntil waitUntil)
  else do
    Process.say "completed sending, entering grace period..."
    loop (Grace client log waitUntil)

loop (Grace client log waitUntil) = do
  done <- Timer.expired waitUntil
  if not done then do
    updated' <- receive log
    case updated' of
      Left updated -> do
        Process.say "no more messages, calculating results..."
        finalise client updated
      Right updated ->
        loop (Grace client updated waitUntil)
  else do
    Process.say "grace period complete, calculating the results we have..."
    finalise client log

-- |
-- Agent starting point. Just waits for an 'Initialise' message.
--
agent :: ProcessId -> Process ()
agent leader =
  Process.expect >>= \message -> case message of
    Initialise processes sendFor waitFor seed -> do
      Process.say "starting..."
      self <- Process.getSelfPid
      sendUntil <- liftIO $ Timer.new sendFor
      random <- Random.new seed
      let
        log = Log.new self
        peers = filter (/= self) processes
        waitUntil = Timer.after waitFor sendUntil
      loop $ Sending leader random log peers sendUntil waitUntil

remotable ['agent]
