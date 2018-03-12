{-# LANGUAGE TemplateHaskell #-}
module Agent.Remote (
    agent
  , __remoteTable
  , agent__static
  , agent__sdict
  , agent__tdict
  ) where

import           Agent.Protocol

import           Control.Distributed.Process (Process, ProcessId)
import qualified Control.Distributed.Process as Process
import           Control.Distributed.Process.Closure (remotable)


agent :: ProcessId -> Process ()
agent client =
  Process.expect >>= \message -> case message of
    Initialise -> do
      self <- Process.getSelfPid
      Process.say "it is alive!"
      Process.send client (Complete self)

remotable ['agent]
