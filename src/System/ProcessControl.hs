{-# LANGUAGE RecordWildCards #-}
module System.ProcessControl where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Monoid
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Process        as P

type ProcessControl = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data TrackedProcess = TrackedProcess { name     :: String
                                     , color    :: String
                                     , execPath :: String
                                     , args     :: [String]
                                     } deriving (Show)

stack :: String
stack = "stack"

process :: TrackedProcess -> IO ProcessControl
process TrackedProcess{..} = createProcess
          (P.proc execPath args)
          { std_out = CreatePipe, std_err = CreatePipe }

data Tracker = Tracker { trackerName :: String, trackedProcess :: ProcessHandle, trackerThreads ::  [ThreadId]  }

data Message = Message { origin :: String, msgColor :: String, msg :: String }

trackProcess :: MVar Message -> TrackedProcess  -> IO (Either IOException Tracker)
trackProcess chan t@TrackedProcess{..} = do
  procStart <- try (process t)
  let readOutput hd = forkIO $ forever $ do
       l <- hGetLine hd
       putMVar chan (Message name color l)
  case procStart of
   Right (Nothing, Just aout, Just aerr, aProc) -> do
     outtid <- readOutput aout
     errtid <- readOutput aerr
     putStrLn $ "\x1b[31mstarted " ++ execPath ++ " " ++ concat (intersperse " " args) ++ "\x1b[0m"
     return $ Right $ Tracker name aProc [outtid, errtid]
   Left e -> print e >> (return $ Left e)

trackProcesses :: [ TrackedProcess ] -> IO ()
trackProcesses toTrack = do
  chan <- newEmptyMVar
  procs <- mapM (trackProcess chan) toTrack
  when (null (lefts procs)) $ forever $ do
             out <- takeMVar chan
             putStrLn $ msgColor out <> "[" <> origin out <> "] - \x1b[0m" <> msg out
