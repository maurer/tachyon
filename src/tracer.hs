import Args
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.IO
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.List
import System.Trace
import Control.Concurrent.STM.BTChan
import Data.Binary.Get

import Trace
import Syscall

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
      Record safe logFile -> do logger   <- makeLogger syscalls log (jDump job)
                                target   <- openFile logFile WriteMode
                                lFinish  <- trace logger safe
                                serialize syscalls target
                                takeMVar lFinish
      Replay unsafe logFile -> do source <- BS.readFile logFile
                                  forkIO $ load source syscalls
                                  emu     <- streamEmu syscalls log (jDump job)
                                  eFinish <- trace emu unsafe
                                  takeMVar eFinish
      Tandem safe unsafe -> do logger <- makeLogger syscalls log (jDump job)
                               print "Starting first trace"
                               lFinish <- trace logger safe
                               print "Building emulator"
                               emu    <- streamEmu syscalls log (jDump job)
                               print "Starting second trace"
                               eFinish <- trace emu unsafe
                               takeMVar lFinish
                               putStrLn "Original finished."
                               takeMVar eFinish
                               putStrLn "Replay finished."

load source c | not (BS.null source) = do
  let (res, source', _) = runGetState get source 0
  flush res c
  load source' c
              | otherwise = return ()

flush [] _ = return ()
flush (x:xs) c = do atomically $ writeBTChan c x
                    flush xs c

serialize :: Binary a => BTChan a -> Handle -> IO ()
serialize serial target = catch (do
  dumpChannel serial target
  serialize serial target) $ \e -> do
    case fromException e of
           Just BlockedIndefinitelyOnSTM -> return () --We're done
           _ -> throw e

dumpChannel :: Binary a => BTChan a -> Handle -> IO ()
dumpChannel serial target = do
  x <- atomically $ readBTChan serial
  rest <- atomically $ readRest serial
  BS.hPutStr target $ encode $ x : rest
  where readRest :: BTChan a -> STM [a]
        readRest serial = do
          mx <- tryReadBTChan serial
          case mx of
            Just x -> do xs <- readRest serial
                         return $ x : xs
            Nothing -> return []
