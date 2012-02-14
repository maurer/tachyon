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
               runTrace th $ traceWithHandler handler) `finally` (putMVar finish ())
  return finish

wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral
