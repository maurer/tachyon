import Args
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.List

import Trace

main = do
   job      <- getJob
   syscalls <- newTChanIO
   case job of
      Record safe logFile -> do logger   <- makeLogger syscalls
                                lFinish  <- trace logger safe
                                takeMVar lFinish
                                --putStrLn "Execution complete, unloading channel"
                                sysList <- atomically $
                                  getCurrentChanContents syscalls
                                --putStrLn "Channel unloaded, writing out"
                                --print $ length sysList
                                BS.writeFile logFile $ encode sysList
                                --putStrLn "Complete."
      Replay unsafe logFile -> do emu     <- streamEmu syscalls
                                  --putStrLn "Beginning decode phase"
                                  sysList <- fmap decode $ BS.readFile logFile
                                  evaluate sysList
                                  --putStrLn "Decoded."
                                  atomically $
                                    mapM (writeTChan syscalls) sysList
                                  --putStrLn "Flushed to channel."
                                  eFinish <- trace emu unsafe
                                  takeMVar eFinish
                                  --putStrLn "Execution complete"

getCurrentChanContents chan = do
   b <- isEmptyTChan chan
   if b
      then return []
      else do x <- readTChan chan
              xs <- getCurrentChanContents chan
              return $ x : xs
