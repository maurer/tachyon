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

import Syscall

trace :: (TPid -> Event -> Trace ()) -> (FilePath, [String]) -> IO (MVar ())
trace handler (exe, args) = do
  finish <- newEmptyMVar
  forkIO $ (do th <- traceExec exe args
               runTrace th $ do
                 exeEntry <- uglyGetEntry exe
                 setBreak exeEntry
                 traceEvent (\_ -> (== Breakpoint))
                 traceWithHandler handler) `finally` (putMVar finish ())
  return finish

wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

uglyGetEntry str = liftIO $ do
  fmap (wordTrace . read) $ readProcess "/bin/bash" ["-c", "readelf -h " ++ str ++ " | grep Entry | awk '{print $4}'"] ""
