import Args
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.List
import System.Trace
import Control.Concurrent.STM.BTChan

import Trace

instance Binary PPid where
  get = fmap (P . toEnum) get
  put (P cpid) = do
    put $ fromEnum cpid

taker k = do
  atomically $ readBTChan k
  taker k

main = do
   job      <- getJob
   let target = jTarget job
   logLock <- newMVar ()
   let log = case jTrace job of
               Nothing -> \_ -> return ()
               Just h  -> \str -> do takeMVar logLock
                                     hPutStr h str
                                     putMVar logLock ()
   syscalls <- newBTChanIO 20
   case target of
   --TODO make record and replay work right again
      Record safe logFile -> do logger   <- makeLogger syscalls log
                                forkIO $ taker syscalls
                                lFinish  <- trace logger safe
                                takeMVar lFinish
                                
                                --putStrLn "Execution complete, unloading channel"
                         {-       sysList <- atomically $
                                  getCurrentChanContents syscalls
                                --putStrLn "Channel unloaded, writing out"
                                --print $ length sysList
                                BS.writeFile logFile $ encode sysList
                                putStrLn "Complete." -}
      Replay unsafe logFile -> do --putStrLn "Beginning decode phase"
                                  sysList <- fmap decode $ BS.readFile logFile
                                  evaluate sysList
                                  --putStrLn "Decoded."
                                  {-
                                  atomically $
                                    mapM (writeTChan syscalls) sysList
                                  --putStrLn "Flushed to channel."
                                  emu     <- streamEmu syscalls
                                  eFinish <- trace emu unsafe
                                  takeMVar eFinish
                                  --putStrLn "Execution complete"
                                  -}
      Tandem safe unsafe -> do logger <- makeLogger syscalls log
                               print "Starting first trace"
                               lFinish <- trace logger safe
                               print "Building emulator"
                               emu    <- streamEmu syscalls log
                               print "Starting second trace"
                               eFinish <- trace emu unsafe
                               takeMVar lFinish
                               putStrLn "Original finished."
                               takeMVar eFinish
                               putStrLn "Replay finished."
getCurrentChanContents chan = do
   b <- isEmptyTChan chan
   if b
      then return []
      else do x <- readTChan chan
              xs <- getCurrentChanContents chan
              return $ x : xs
