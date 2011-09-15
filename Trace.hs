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

import Syscall

trace :: (Event -> Trace ()) -> (FilePath, [String]) -> IO (MVar ())
trace handler (exe, args) = do
  finish <- newEmptyMVar
  forkIO $ (do th <- traceExec exe args
               runTrace th $ do
                 exeEntry <- uglyGetEntry exe
                 setBreak exeEntry
                 traceEvent (== Breakpoint)
                 traceWithHandler handler) `finally` (putMVar finish ())
  return finish

wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

uglyGetEntry _ = return $ wordTrace 0x402980
