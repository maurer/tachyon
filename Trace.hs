module Trace 
(trace
,makeLogger
,streamEmu
,Syscall(..)
) where

import System.Trace
import Control.Concurrent
import Data.Binary
import TracerLoop
import Control.Exception
import Foreign.Ptr
import System.Process
import Control.Monad.IO.Class
import System.Posix.Process
import Syscall

trace :: (TPid -> Event -> Trace ()) -> (FilePath, [String]) -> IO (MVar ())
trace handler (exe, args) = do
  finish <- newEmptyMVar
  forkIO $
           (do print "Trying to trace"
               th <- traceExec exe args
               print "Trace activating"
              -- runTrace th $ traceWithHandler handler) {-
               runTrace th $ do
                 exeEntry <- uglyGetEntry exe
                 setBreak exeEntry
                 liftIO $ print "Waiting for breakpoint"
                 traceEvent (\_ -> (== Breakpoint))
                 liftIO $ print "breakpoint"
                 traceWithHandler handler) `finally` (putMVar finish ())
  return finish

wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

uglyGetEntry str = liftIO $ do
  fmap (wordTrace . read) $ readProcess "/bin/bash" ["-c", "readelf -h " ++ str ++ " | grep Entry | awk '{print $4}'"] ""
