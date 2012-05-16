module TracerLoop where

import System.Trace
import Syscall
import Data.IORef
import Control.Monad.IO.Class
import TypedSyscalls
import Control.Concurrent.STM
import Util
import Data.Int
import qualified Data.Map as Map
import System.Exit
import Control.Concurrent.STM.BTChan
import Data.Binary
import Data.Word
import Foreign.Ptr
import Control.Concurrent.MVar

logFormat xs = (concatMap (\x -> "[" ++ x ++ "]") (init xs)) ++ ": " ++ (last xs) ++ "\n"

formatIn = show

instance Binary WordPtr where
  get = fmap fromIntegral $ (get :: Get Word64)
  put x = put $ ((fromIntegral x) :: Word64)

data Context a = Ctx {ctxWriteTLS :: TPid -> a -> Trace ()
                     ,ctxReadTLS  :: TPid -> Trace a
                     ,ctxCore     :: TPid -> Trace ()
                     ,ctxLog      :: TPid -> String -> Trace ()
                     ,ctxRegTh    :: TPid -> TPid -> Trace ()
                     ,ctxDecTh    :: TPid -> Trace TPid
                     ,ctxEncTh    :: TPid -> Trace TPid
                     }

makeContext :: String -> (String -> IO ()) -> Bool -> IO (Context a)
makeContext prefix rawLog coreDumps = do
  coreNum <- newMVar 0
  tls <- newMVar Map.empty
  ttt <- newMVar (Map.empty, Map.empty)
  let writeTLS t v = liftIO $ do
        tls' <- takeMVar tls
        putMVar tls (Map.insert t v tls')
  let readTLS t = liftIO $ fmap (Map.! t) $ readMVar tls
  let logStr x = liftIO $ rawLog x
  let log tpid s = logStr $ logFormat [prefix, (show tpid), s]
  let dumpCore tpid | coreDumps = do
        c <- core
        log tpid "cored!"
        v <- liftIO $ modifyMVar coreNum (\x -> return (x + 1, x))
        liftIO $ encodeFile (prefix ++ "." ++ (show tpid) ++ "." ++ (show v)) c
                    | otherwise = return ()
  let registerThread t t' = liftIO $ do
         (ttt', ttz') <- takeMVar ttt
         putMVar ttt $ (Map.insert t t' ttt', Map.insert t' t ttz')
  let decodeThread t = liftIO $ do
         (ttt', _) <- readMVar ttt
         return (ttt' Map.! t)
  let encodeThread t = liftIO $ do
         (_, ttt') <- readMVar ttt
         return (ttt' Map.! t)
  return $ Ctx {ctxWriteTLS = writeTLS
               ,ctxReadTLS  = readTLS
               ,ctxCore     = dumpCore
               ,ctxLog      = log
               ,ctxRegTh    = registerThread
               ,ctxDecTh    = decodeThread
               ,ctxEncTh    = encodeThread}

loggerHandler :: BTChan (TPid, Syscall) -> Context SysReq -> (TPid -> Event -> Trace ())
loggerHandler syscalls ctx tpid e = do
   let log = ctxLog ctx tpid
   let dumpCore = ctxCore ctx tpid
   let readTLS = ctxReadTLS ctx tpid
   let writeTLS = ctxWriteTLS ctx tpid
   let writeSys x = liftIO $ atomically $ writeBTChan syscalls x
   case e of
      PreSyscall  -> do log "Coring pre"
                        dumpCore
                        log "Done coring pre"
                        sysIn <- readInput
                        log $ formatIn sysIn
                        case sysIn of
                           (SysReq ExitGroup _) -> liftIO $
                               atomically $ writeBTChan syscalls $
                                  (tpid, Syscall sysIn (SysRes 0 []))
                                       
                           _ -> writeTLS sysIn
      PostSyscall -> do sysIn  <- readTLS
                        sys    <- readOutput
                        writeSys (tpid, Syscall sysIn sys)
                        log "Coring post"
                        dumpCore
                        log "Done coring post"
      Signal x -> do error $ "SIGNAL: " ++ (show x)
                     if (x == 11)
                        then error "Segfault encountered."
                        else return ()
      Exit _ -> log $ "ThreadExit"
      Split tp -> do log $ "Split: " ++ (show tp)
                     sysIn <- readTLS
                     ctxWriteTLS ctx tp sysIn
makeLogger :: BTChan (TPid, Syscall) -> (String -> IO ()) -> Bool -> IO (Trace (), TPid -> Event -> Trace ())
makeLogger syscalls rawLog coreDumps = do
  ctx <- makeContext "logger" rawLog coreDumps
  return (return (), loggerHandler syscalls ctx)

ignoreit _ _ = return ()
--sysc = id
sysc (SysReq n _) = n
streamEmu :: BTChan (TPid, Syscall) -> (String -> IO ()) -> Bool -> IO (Trace (), (TPid -> Event -> Trace ()))
streamEmu syscalls rawLog coreDumps = do
  ctx <- makeContext "emulator" rawLog coreDumps
  let registerThread = ctxRegTh ctx
  let encodeThread = ctxEncTh ctx
  let decodeThread = ctxDecTh ctx
  print "Waiting for first syscall..."
  z@(t0,_) <- atomically $ readBTChan syscalls
  print "Got it!"
  atomically $ unGetBTChan syscalls z
  print "Put it back."
  let self = \tpid e -> do
             let log = ctxLog ctx tpid
             let dumpCore = ctxCore ctx tpid
             let readTLS = ctxReadTLS ctx tpid
             let writeTLS = ctxWriteTLS ctx tpid
             case e of
                   Split newtid -> do --Assume that a clone is being
                                      --processed
                     z@((Syscall _ (SysRes x _)), _, _) <- readTLS
                     registerThread newtid (buildTPid x)
                     ctxWriteTLS ctx (buildTPid x) z
                   PreSyscall -> do
                     sysIn <- readInput
                     log $ formatIn sysIn
                     t <- decodeThread tpid
                     dumpCore
                     regs <- getRegs
                     ce@(t', sys@(Syscall i o)) <- liftIO $ atomically $ readBTChan
                                              syscalls
                     sys' <- case sysIn of
                         (SysReq MMap xs) -> do
                           setRegs $ regs { orig_rax = 9,
                                        --    rdi = 0,
                                            r8 = bitCast $ (-1 :: Int64),
                                            r9 = 0,
                                            rdx = 7,
                                            r10 = 34 }
                           return (SysReq SafeMMap xs)
                         _ -> return sysIn
                     rbak <- getRegs
                     if not $ passthrough sys' then nopSyscall else return ()
                     writeTLS (sys, sys', orig_rax regs)
                     if t == t' --Our current thread is the executing thread
                        then if not $ compat i sysIn
                               then do log "Incompatible"
                                       x <- streamRewrite t sysIn sys syscalls
                                       if x
                                          then do setRegs rbak
                                                  self tpid e
                                          else do liftIO $ print (i, sysIn)
                                                  error "Replace me with clean death" --syscalls sys
                               else return ()
                        else do tz' <- encodeThread t'
                                liftIO $ atomically $ unGetBTChan syscalls ce
                                sleep
                                wakeUp tz'
                   PostSyscall -> do
                     t <- decodeThread tpid
                     (sys@(Syscall i o), sysIn, orax) <- readTLS
                     regs <- getRegs
                     setRegs $ regs {orig_rax = orax}
                     case i of
                       (SysReq Clone _) -> writeOutput o
                       (SysReq MMap _)  -> case o of
                                             SysRes _ vs -> writeOutput $ SysRes (rax regs) vs
                       _ -> if not $ passthrough i then writeOutput o else return ()
                     dumpCore
                   Signal 11 -> error "Segfault encountered"
                   x -> liftIO $ print x
  return (return (), self)

streamRewrite tpid z@(SysReq SetSockOpt _) y syscalls = do
  liftIO $ atomically $ do unGetBTChan syscalls (tpid, y)
                           unGetBTChan syscalls (tpid, (Syscall z (SysRes 0 [])))
  return True
streamRewrite _ _ _ _ = return False

buildTPid = P . fromIntegral
compat (SysReq Connect _) (SysReq Connect _) = True -- Lie
compat (SysReq RTSigAction _) (SysReq RTSigAction _) = True
compat (SysReq MUnmap _) (SysReq MUnmap _) = True
compat (SysReq MProtect _) (SysReq MProtect _) = True
compat x y = x == y

--compat (SysReq n _) (SysReq n' _) = n == n' --Woefully insufficient, but sanity
--OK
passthrough (SysReq SafeMMap _) = True
passthrough (SysReq MUnmap _) = True
passthrough (SysReq Brk _) = True
passthrough (SysReq ExitGroup _) = True
passthrough (SysReq Clone _) = True
--Maybe not OK
passthrough (SysReq SetArchPrCtl _) = True
--passthrough (SysReq Write _) = True
--Not OK
{-
passthrough (SysReq MMap _) = True
passthrough (SysReq Open _) = True
passthrough (SysReq Close _) = True
passthrough (SysReq ChDir _) = True
-}
--passthrough _ = True
{-
passthrough (Syscall (SysReq GetDEnts _) _) = False
passthrough _ = True
-}
passthrough _ = False

-- Deathland: 140527726078759
--140737351985959

--0x7ffff7ffe050
