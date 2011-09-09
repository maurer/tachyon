module TracerLoop where

import System.Trace
import Syscall
import Data.IORef
import Control.Monad.IO.Class
import TypedSyscalls
import Control.Concurrent.STM
import Util
import Data.Int

makeLogger :: TChan Syscall -> IO (Event -> Trace ())
makeLogger syscalls = do
  req <- newIORef undefined
  return $ \e -> case e of
                   PreSyscall  -> do sysIn  <- readInput
                                     case sysIn of
                                       (SysReq ExitGroup _) -> liftIO $
                                         atomically $ writeTChan syscalls $
                                           Syscall sysIn (SysRes 0 [])
                                       _ -> liftIO $ writeIORef req sysIn
                   PostSyscall -> do sysIn  <- liftIO $ readIORef req
                                     sys    <- readOutput sysIn
                                     liftIO $ atomically $ writeTChan
                                       syscalls $ sys
                   Signal x -> do liftIO $ print x
                                  return ()
                   Exit _ -> return ()

streamEmu :: TChan Syscall -> IO (Event -> Trace ())
streamEmu syscalls = do
  req <- newIORef undefined
  return $ \e -> case e of
                   PreSyscall -> do
                     sysIn <- readInput
                     sys@(Syscall i o) <- liftIO $ atomically $ readTChan
                                              syscalls
                     if not $ compat i sysIn
                       then liftIO $ atomically $ unGetTChan syscalls sys
                       else return ()
                     liftIO $ writeIORef req (sys, sysIn)
                {-     sys' <- case i of
                         (SysReq MMap xs) -> do
                           regs <- getRegs
                           setRegs $ regs { orig_rax = 9,
                                            rdi = 0,
                                            r8 = bitCast $ (-1 :: Int32),
                                            rdx = 7,
                                            r10 = 34 }
                           return (Syscall (SysReq SafeMMap xs) o)
                         _ -> return sys -}
                     if not $ passthrough sys then nopSyscall else return ()
                   PostSyscall -> do
                     (sys@(Syscall _ o), sysIn) <- liftIO $ readIORef req
                     if not $ passthrough sys then writeOutput sysIn o else return ()
                   Signal x -> do liftIO $ print x
                                  return ()
                   Exit _ -> return ()

compat (SysReq n _) (SysReq n' _) = n == n' --Woefully insufficient, but sanity
--OK
passthrough (Syscall (SysReq SafeMMap _) _) = True
passthrough (Syscall (SysReq MUnmap _) _) = True
passthrough (Syscall (SysReq Brk _) _) = True
passthrough (Syscall (SysReq ExitGroup _) _) = True
--Maybe not OK
passthrough (Syscall (SysReq SetArchPrCtl _) _) = True
passthrough (Syscall (SysReq Write _) _) = True
--Totally not OK stuff
passthrough (Syscall (SysReq Open _) _) = True
passthrough (Syscall (SysReq MMap _) _) = True
{-
passthrough (Syscall (SysReq GetDEnts _) _) = False
passthrough _ = True


-}
passthrough _ = False

-- Deathland: 140527726078759
--140737351985959

--0x7ffff7ffe050
