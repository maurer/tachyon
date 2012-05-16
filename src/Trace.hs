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

-- | Takes in a our definition of a tracing method, and a target, and runs it.
trace :: (Trace (), TPid -> Event -> Trace ()) -- ^(initializer, handler)
      -> (FilePath, [String])                  -- ^(elf to load, arguments)
      -> IO (MVar ())                          -- ^MVar which will be placed
                                               --  on completion.
trace (initializer, handler) (exe, args) = do
  finish <- newEmptyMVar
  forkIO $ do th <- traceExec exe args
              runTrace th $ do initializer
                               traceWithHandler handler
              putMVar finish ()
  return finish
