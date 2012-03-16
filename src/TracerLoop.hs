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

logFormat xs = (concatMap (\x -> "[" ++ x ++ "]") (init xs)) ++ ": " ++ (last xs) ++ "\n"

formatIn = show

instance Binary WordPtr where
  get = fmap fromIntegral $ (get :: Get Word64)
  put x = put $ ((fromIntegral x) :: Word64)

makeLogger :: BTChan (TPid, Syscall) -> (String -> IO ()) -> Bool -> IO (TPid -> Event -> Trace ())
makeLogger syscalls rawLog coreDumps = do
  coreNum <- newIORef 0
  let logStr x = liftIO $ rawLog x
  tls <- newIORef Map.empty
  let writeTLS t v = do
        tls' <- readIORef tls
        case Map.lookup t tls' of
           Just z  -> writeIORef z v
           Nothing -> do z <- newIORef v
                         writeIORef tls (Map.insert t z tls')
  let readTLS t = do v <- fmap (Map.! t) $ readIORef tls
                     readIORef v
  return $ \tpid e -> do
             let log s = logStr $ logFormat ["logger", (show tpid), s]
             let dumpCore | coreDumps = do
                   c <- core
                   log "cored!"
                   v <- liftIO $ atomicModifyIORef coreNum (\x -> (x + 1, x))
                   liftIO $ encodeFile ("logger." ++ (show tpid) ++ "." ++ (show v)) c
                          | otherwise = return ()
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
                                       
                                       _ -> liftIO $ writeTLS tpid sysIn
                   PostSyscall -> do sysIn  <- liftIO $ readTLS tpid
                                     sys    <- readOutput
                                     
                                     --liftIO $ print (sysIn, sys)
                                     liftIO $ atomically $ writeBTChan
                                       syscalls $ (tpid, Syscall sysIn sys)
                                     log "Coring post"
                                     dumpCore
                                     log "Done coring post"
                   Signal x -> do error $ "SIGNAL: " ++ (show x)
                                  if (x == 11)
                                     then error "Segfault encountered."
                                     else return ()
                   Exit _ -> log $ "ThreadExit"
                   Split tp -> do log $ "Split: " ++ (show tp)
                                  sysIn <- liftIO $ readTLS tpid
                                  liftIO $ writeTLS tp sysIn

ignoreit _ _ = return ()
--sysc = id
sysc (SysReq n _) = n
streamEmu :: BTChan (TPid, Syscall) -> (String -> IO ()) -> Bool -> IO (TPid -> Event -> Trace ())
streamEmu syscalls rawLog coreDumps = do
  coreNum <- newIORef 0
  let logStr x = liftIO $ rawLog x
  tls <- newIORef Map.empty
  ttt <- newIORef (Map.empty, Map.empty)
  let writeTLS t v = do
        tls' <- readIORef tls
        case Map.lookup t tls' of
           Just z  -> writeIORef z v
           Nothing -> do z <- newIORef v
                         writeIORef tls (Map.insert t z tls')
  let readTLS t = do tls' <- readIORef tls
                     let v = tls' Map.! t
                     readIORef v
  let registerThread t t' = do
         (ttt', ttz') <- readIORef ttt
         writeIORef ttt $ (Map.insert t t' ttt', Map.insert t' t ttz')
  let decodeThread t = do
         (ttt', _) <- readIORef ttt
         return (ttt' Map.! t)
  let encodeThread t = do
         (_, ttt') <- readIORef ttt
         return (ttt' Map.! t)
  print "Waiting for first syscall..."
  z@(t0,_) <- atomically $ readBTChan syscalls
  print "Got it!"
  atomically $ unGetBTChan syscalls z
  print "Put it back."
  let emptyReg t t' = do
        (ttt', _) <- readIORef ttt
        if Map.null ttt' then registerThread t t' else return ()
  let self = \tpid e -> do
             let log x = logStr $ logFormat ["emulate", (show tpid), x]
             let dumpCore | coreDumps = do
                   c <- core
                   log "cored!"
                   v <- liftIO $ atomicModifyIORef coreNum (\x -> (x + 1, x))
                   liftIO $ encodeFile ("emulate." ++ (show tpid) ++ "." ++ (show v)) c
                          | otherwise = return ()
             case e of
                   Split newtid -> do --Assume that a clone is being
                                      --processed
                     t <- liftIO $ decodeThread tpid
                     z@((Syscall _ (SysRes x _)), _, _) <- liftIO $ readTLS t
                     liftIO $ registerThread newtid (buildTPid x)
                     liftIO $ writeTLS (buildTPid x) z
                   PreSyscall -> do
                     liftIO $ emptyReg tpid t0
                     sysIn <- readInput
                     log $ formatIn sysIn
                     t <- liftIO $ decodeThread tpid
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
                     liftIO $ writeTLS t (sys, sys', orig_rax regs)
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
                        else do tz' <- liftIO $ encodeThread t'
                                liftIO $ atomically $ unGetBTChan syscalls ce
                                sleep
                                wakeUp tz'
                   PostSyscall -> do
                     t <- liftIO $ decodeThread tpid
                     (sys@(Syscall i o), sysIn, orax) <- liftIO $ readTLS t
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
  return self

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
