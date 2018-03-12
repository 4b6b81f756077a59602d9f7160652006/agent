{-# LANGUAGE TemplateHaskell #-}

import           Agent.Protocol
import           Agent.Remote

import           Control.Applicative ((<**>))
import qualified Control.Distributed.Process as Process
import           Control.Distributed.Process.Closure (mkClosure)
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as Backend
import           Control.Monad (unless)

import           Data.Foldable (for_)
import           Data.Traversable (for)
import           Data.Monoid ((<>))

import           Network.Socket (HostName, ServiceName)

import           Options.Applicative (Parser, execParser, info, helper)
import           Options.Applicative (long, value, strOption, flag)


data Leader =
  Leader

data Arguments =
  Arguments (Maybe Leader) HostName ServiceName

parser :: Parser Arguments
parser =
  Arguments
    <$> flag (Nothing) (Just Leader) (long "leader")
    <*> (strOption (long "host" <> value "127.0.0.1"))
    <*> (strOption (long "port" <> value "0"))

follower :: Backend.Backend -> IO ()
follower backend =
  Backend.startSlave backend

leader :: Backend.Backend -> Leader -> IO ()
leader backend Leader =
  Backend.startMaster backend $ \nodes -> do
    self <- Process.getSelfPid
    pids <- for nodes $ \n -> do
      Process.spawn n ($(mkClosure 'agent) self)
    for_ pids $ \pid -> do
      Process.send pid Initialise
    let
      wait remaining = do
        Complete pid <- Process.expect
        let result = filter (/= pid) remaining
        Process.say . mconcat $ ["complete: pid[", show pid, "]"]
        Process.say . mconcat $ ["remaining: pids", show result]
        unless (null result) $
          wait result
    wait pids

main :: IO ()
main =
  execParser (info (parser <**> helper) mempty) >>= \arguments -> case arguments of
    Arguments mode host port -> do
      backend <- Backend.initializeBackend host port $
        __remoteTable Node.initRemoteTable
      maybe (follower backend) (leader backend) mode
