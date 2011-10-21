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

makeLogger :: TChan (TPid, Syscall) -> IO (TPid -> Event -> Trace ())
makeLogger syscalls = do
  tls <- newIORef Map.empty
  let writeTLS t v = do
        tls' <- readIORef tls
        case Map.lookup t tls' of
           Just z  -> writeIORef z v
           Nothing -> do z <- newIORef v
                         writeIORef tls (Map.insert t z tls')
  let readTLS t = do putStrLn "readTLS"
                     v <- fmap (Map.! t) $ readIORef tls
                     putStrLn "mid"
                     readIORef v
  return $ \tpid e ->
             case e of
                   PreSyscall  -> do sysIn <- readInput
                                     liftIO $ print sysIn
                                     case sysIn of
                                       (SysReq ExitGroup _) -> liftIO $
                                         atomically $ writeTChan syscalls $
                                           (tpid, Syscall sysIn (SysRes 0 []))
                                       
                                       _ -> liftIO $ writeTLS tpid sysIn
                   PostSyscall -> do sysIn  <- liftIO $ readTLS tpid
                                     liftIO $ putStrLn $ "Attempting to read for " ++ (show sysIn)
                                     sys    <- readOutput sysIn
                                     liftIO $ putStrLn $ "Success."
                                     liftIO $ atomically $ writeTChan
                                       syscalls $ (tpid, sys)
                   Signal x -> do liftIO $ print x
                                  return ()
                   Exit _ -> return ()
                   Split tp -> do sysIn <- liftIO $ readTLS tpid
                                  liftIO $ writeTLS tp sysIn

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
  let readTLS t = do putStrLn "readTLS"
                     tls' <- readIORef tls
                     print (Map.keys tls', t)
                     let v = tls' Map.! t
                     readIORef v
  let registerThread t t' = do
         putStrLn $ "Registering thread " ++ (show t) ++ " as " ++ (show t')
         (ttt', ttz') <- readIORef ttt
         writeIORef ttt $ (Map.insert t t' ttt', Map.insert t' t ttz')
  let decodeThread t = do
         putStrLn $ "Attempting to decode thread " ++ (show t)
         (ttt', _) <- readIORef ttt
         print ttt'
         print (ttt' Map.! t)
         return (ttt' Map.! t)
  let encodeThread t = do
         putStrLn $ "Attempting to encode thread " ++ (show t)
         (_, ttt') <- readIORef ttt
         return (ttt' Map.! t)
  z@(t0,_) <- atomically $ readTChan syscalls
  print "Hear"
  atomically $ unGetTChan syscalls z
  let emptyReg t t' = do
        (ttt', _) <- readIORef ttt
        if Map.null ttt' then registerThread t t' else return ()
  return $ \tpid e -> case e of
                   Split newtid -> do --Assume that a clone is being
                                      --processed
                     t <- liftIO $ decodeThread tpid
                     z@((Syscall _ (SysRes x _)), _) <- liftIO $ readTLS t
                     liftIO $ registerThread newtid (buildTPid x)
                     liftIO $ writeTLS (buildTPid x) z
                   PreSyscall -> do
                     liftIO $ emptyReg tpid t0
                     sysIn <- readInput
                     t <- liftIO $ decodeThread tpid
                     ce@(t', sys@(Syscall i o)) <- liftIO $ atomically $ readTChan
                                              syscalls
                     sys' <- case sysIn of
                         (SysReq MMap xs) -> do
                           regs <- getRegs
                           setRegs $ regs { orig_rax = 9,
                                            rdi = 0,
                                            r8 = bitCast $ (-1 :: Int32),
                                            rdx = 7,
                                            r10 = 34 }
                           return (SysReq SafeMMap xs)
                         _ -> return sysIn
                     if not $ passthrough sys' then nopSyscall else return ()
                     liftIO $ writeTLS t (sys, sysIn)
                     if t == t' --Our current thread is the executing thread
                        then if not $ compat i sysIn
                               then do liftIO $ print (i, sysIn)
                                       error "Replace me with clean death" syscalls sys
                               else return ()
                        else do tz' <- liftIO $ encodeThread t'
                                liftIO $ atomically $ unGetTChan syscalls ce
                                contextSwitch tz'
                   PostSyscall -> do
                     t <- liftIO $ decodeThread tpid
                     (sys@(Syscall i o), sysIn) <- liftIO $ readTLS t
                     if not $ passthrough i then writeOutput sysIn o else return ()
                     case i of
                       (SysReq Clone _) -> writeOutput sysIn o
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
passthrough (SysReq Write _) = True
{-
passthrough (Syscall (SysReq GetDEnts _) _) = False
passthrough _ = True


-}
passthrough _ = False

-- Deathland: 140527726078759
--140737351985959

--0x7ffff7ffe050
