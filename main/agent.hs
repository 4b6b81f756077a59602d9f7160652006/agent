{-# LANGUAGE TemplateHaskell #-}

import           Agent.Data.Random (Seed (..))
import           Agent.Data.Timer (Duration (..))
import           Agent.Protocol
import           Agent.Remote

import           Control.Applicative ((<**>))
import qualified Control.Distributed.Process as Process
import           Control.Distributed.Process.Closure (mkClosure)
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as Backend
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Foldable (for_)
import           Data.Traversable (for)
import           Data.Monoid ((<>))
import qualified Data.Vector as Vector

import           Network.Socket (HostName, ServiceName)

import           Options.Applicative (Parser, execParser, info, helper)
import           Options.Applicative (long, option, auto, value, strOption, flag, optional)

import qualified System.Random.MWC as Random


data Terminate =
    Terminate
  | DoNotTerminate
    deriving Eq

data Leader =
  Leader Terminate Duration Duration Seed

data Arguments =
  Arguments (Maybe Leader) HostName ServiceName

parser :: Parser Arguments
parser =
  Arguments
    <$> optional (Leader
      <$> flag DoNotTerminate Terminate (long "terminate-nodes-on-completion")
      <*> (Duration <$> option auto (long "send-for" <> value 1))
      <*> (Duration <$> option auto (long "wait-for" <> value 1))
      <*> (Seed <$> option auto (long "with-seed" <> value 0)))
    <*> (strOption (long "host" <> value "127.0.0.1"))
    <*> (strOption (long "port" <> value "0"))

follower :: Backend.Backend -> IO ()
follower backend =
  Backend.startSlave backend

leader :: Backend.Backend -> Leader -> IO ()
leader backend (Leader terminate sendFor waitFor seed) =
  Backend.startMaster backend $ \nodes -> do
    self <- Process.getSelfPid
    gen <- liftIO $ Random.initialize (Vector.singleton $ getSeed seed)
    pids <- for nodes $ \n -> do
      Process.spawn n ($(mkClosure 'agent) self)
    for_ pids $ \pid -> do
      split <- liftIO $ Seed <$> Random.uniform gen
      Process.send pid (Initialise pids sendFor waitFor split)
    let
      wait remaining = do
        Complete pid messages total <- Process.expect
        let result = filter (/= pid) remaining
        Process.say . mconcat $ [
            "complete: pid[", show pid, "], "
          , "messages = ", show messages, ", "
          , "total = ", show total
          ]
        Process.say . mconcat $ ["remaining: pids", show result]
        unless (null result) $
          wait result
    wait pids
    when (terminate == Terminate) $
      for_ nodes $ \n -> do
        Backend.terminateSlave n

main :: IO ()
main =
  execParser (info (parser <**> helper) mempty) >>= \arguments -> case arguments of
    Arguments mode host port -> do
      backend <- Backend.initializeBackend host port $
        __remoteTable Node.initRemoteTable
      maybe (follower backend) (leader backend) mode
