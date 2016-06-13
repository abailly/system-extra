{-# LANGUAGE RecordWildCards #-}
module System.ProcessControl where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Monoid
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Process          as P

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

instance Show Tracker where
  show Tracker{..} = "Tracker { trackerName = " ++ trackerName ++ ", trackerThreads = "++ show trackerThreads ++ "}"

data Message = Message { origin :: String, msgColor :: String, msg :: String }

trackProcess :: Chan Message -> TrackedProcess  -> IO (Either IOException Tracker)
trackProcess chan t@TrackedProcess{..} = do
  procStart <- try (process t)
  case procStart of
   Right (Nothing, Just aout, Just aerr, aProc) -> do
     outtid <- readOutput aout
     errtid <- readOutput aerr
     putStrLn $ "\x1b[31mstarted " ++ name ++ " (" ++ execPath ++ " " ++ concat (intersperse " " args) ++ ")\x1b[0m"
     let tr = Tracker name aProc [outtid, errtid]
     return $ Right tr
   Left e -> print e >> (return $ Left e)

   where
     readOutput hd = forkIO $ forever $ do
       l <- hGetLine hd
       writeChan chan (Message name color l)

trackProcesses :: [ TrackedProcess ] -> IO ()
trackProcesses toTrack = do
  chan <- newChan
  procs <- mapM (trackProcess chan) toTrack
  when (null (lefts procs)) $ forever $ do
    out <- readChan chan
    putStrLn $ msgColor out <> "[" <> origin out <> "] - \x1b[0m" <> msg out
