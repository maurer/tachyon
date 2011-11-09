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

makeLogger :: TChan (TPid, Syscall) -> IO (TPid -> Event -> Trace ())
makeLogger syscalls = do
  tls <- newIORef Map.empty
  let writeTLS t v = do
        tls' <- readIORef tls
        case Map.lookup t tls' of
           Just z  -> writeIORef z v
           Nothing -> do z <- newIORef v
                         writeIORef tls (Map.insert t z tls')
  let readTLS t = do v <- fmap (Map.! t) $ readIORef tls
                     readIORef v
  return $ \tpid e ->
             case e of
                   PreSyscall  -> do sysIn <- readInput
                                     case sysIn of
                                       (SysReq ExitGroup _) -> liftIO $
                                         atomically $ writeTChan syscalls $
                                           (tpid, Syscall sysIn (SysRes 0 []))
                                       
                                       _ -> liftIO $ writeTLS tpid sysIn
                   PostSyscall -> do sysIn  <- liftIO $ readTLS tpid
                                     sys    <- readOutput
                                     --liftIO $ print (sysIn, sys)
                                     liftIO $ atomically $ writeTChan
                                       syscalls $ (tpid, Syscall sysIn sys)
                   Signal x -> do liftIO $ putStrLn $ "SIGNAL: " ++ (show x)
                                  return ()
                   Exit _ -> liftIO $ putStrLn $ "ThreadExit: " ++ (show tpid)
                   Split tp -> do sysIn <- liftIO $ readTLS tpid
                                  liftIO $ writeTLS tp sysIn

ignoreit _ _ = return ()

streamEmu :: TChan (TPid, Syscall) -> IO (TPid -> Event -> Trace ())
streamEmu syscalls = do
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
  z@(t0,_) <- atomically $ readTChan syscalls
  atomically $ unGetTChan syscalls z
  let emptyReg t t' = do
        (ttt', _) <- readIORef ttt
        if Map.null ttt' then registerThread t t' else return ()
  return $ \tpid e -> case e of
                   Split newtid -> do --Assume that a clone is being
                                      --processed
                     t <- liftIO $ decodeThread tpid
                     z@((Syscall _ (SysRes x _)), _, _) <- liftIO $ readTLS t
                     liftIO $ registerThread newtid (buildTPid x)
                     liftIO $ writeTLS (buildTPid x) z
                   PreSyscall -> do
                     liftIO $ emptyReg tpid t0
                     sysIn <- readInput
                     t <- liftIO $ decodeThread tpid
                     regs <- getRegs
                     ce@(t', sys@(Syscall i o)) <- liftIO $ atomically $ readTChan
                                              syscalls
                     sys' <- case sysIn of
                         (SysReq WriteV _) -> do
                            traceWithHandler ignoreit
                            error "boom"
                         (SysReq MMap xs) -> do
                           setRegs $ regs { orig_rax = 9,
                                            rdi = 0,
                                            r8 = bitCast $ (-1 :: Int32),
                                            rdx = 7,
                                            r10 = 34 }
                           return (SysReq SafeMMap xs)
                         _ -> return sysIn
                     if not $ passthrough sys' then nopSyscall else return ()
                     liftIO $ writeTLS t (sys, sys', orig_rax regs)
                     if t == t' --Our current thread is the executing thread
                        then if not $ compat i sysIn
                               then do liftIO $ print (i, sysIn)
                                       liftIO $ exitWith ExitSuccess
                                       --error "Replace me with clean death" syscalls sys
                               else return ()
                        else do tz' <- liftIO $ encodeThread t'
                                liftIO $ atomically $ unGetTChan syscalls ce
                                sleep
                                wakeUp tz'
                   PostSyscall -> do
                     t <- liftIO $ decodeThread tpid
                     (sys@(Syscall i o), sysIn, orax) <- liftIO $ readTLS t
                     regs <- getRegs
                     setRegs $ regs {orig_rax = orax}
                     if not $ passthrough i then writeOutput o else return ()
                     case i of
                       (SysReq Clone _) -> writeOutput o
                       _ -> return ()
                   _ -> return ()

buildTPid = P . fromIntegral

compat (SysReq n _) (SysReq n' _) = n == n' --Woefully insufficient, but sanity
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
--passthrough (SysReq MMap _) = True
--passthrough (SysReq Open _) = True
--passthrough _ = True
{-
passthrough (Syscall (SysReq GetDEnts _) _) = False
passthrough _ = True
-}
passthrough _ = False

-- Deathland: 140527726078759
--140737351985959

--0x7ffff7ffe050
