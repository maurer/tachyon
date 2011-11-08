module TypedSyscalls where

import Syscall
import System.Trace
import qualified Data.Map as Map
import SyscallTable
import Data.Word
import Control.Monad.IO.Class
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Data.Bits
import qualified Data.ByteString as BS

callConv :: [Regs -> Word64]
callConv = [rdi, rsi, rdx, r10, r8, r9]

readInput :: Trace SysReq
readInput = do
  regs <- getRegs
  let sid =  syscallID regs
  liftIO $ print sid
  let sigs = getSig sid
  let args = map ($ regs) callConv
  liftIO $ print args
  fmap (SysReq sid) $ zipWithM (readArg args) args sigs

getSig :: SyscallID -> [ArgType]
getSig sid =
  case Map.lookup sid syscallTable of
    Just (SysSig sigs) -> sigs
    Nothing  -> error $ "Unimplemented syscall: " ++ (show sid)

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence $ zipWith3 f xs ys zs

readOutput :: SysReq -> Trace Syscall
readOutput req@(SysReq i args) = do
  regs <- getRegs
  let ret = rax regs
  ress <- zipWithM3 (readRes ret args) args (map ($ regs) callConv) (getSig i)
  liftIO $ putStrLn "All arguments read."
  return $ Syscall req (SysRes ret ress)

writeOutput :: SysReq -> SysRes -> Trace ()
writeOutput (SysReq i args) (SysRes r ress) = do
  regs <- getRegs
  setRegs $ regs {rax = r}
  -- Note, StorageReccomend is only legal for calls which get passed through
  zipWith3M_ (writeRes $ rax regs) (map ($regs) callConv) (getSig i) ress
  where zipWith3M_ f a b c = sequence_ $ zipWith3 f a b c

writeRes :: Word64 -> Word64 -> ArgType -> Maybe BS.ByteString -> Trace ()
writeRes ret addr (StorageReccomend _) (Just bs) =
  if (addr == 0)
    then writeRes ret ret (Storage undefined) (Just bs)
    else writeRes ret addr (Storage undefined) (Just bs)
writeRes _ addr _ (Just bs) = do
  liftIO $ print bs
  writeByteString bs (wordTrace addr)
writeRes _ _ _ _ = return ()

readRes :: Word64 -> [SysReqArg] -> SysReqArg -> Word64 -> ArgType -> Trace (Maybe BS.ByteString)
readRes ret args arg p ty = do
  liftIO $ putStrLn $ "Reading: " ++ (show ty)
  readRes' ret args arg p ty
readRes' ret args arg p ty = case ty of
  Small -> return Nothing
  SmallSize -> return Nothing
  InOut sz -> readRes ret args arg p (Storage sz)
  Storage sz -> if True --(ret > maxBound - 1024)
                   then do sz' <- size args sz
                           liftIO $ print p
                           liftIO $ print sz'
                           v <- fmap Just $ readByteString (wordTrace p) sz'
  --                         liftIO $ print v
                           return v
                   else return Nothing
  MaybeStorage sz -> let SmallVal p = arg
                     in if (p == 0) then return Nothing else readRes ret args arg p (Storage sz)
  StorageReccomend sz -> if (ret > maxBound - 1024)
                            then return Nothing
                            else do sz' <- size args sz
                                    v <- fmap Just $ readByteString (wordTrace ret) sz'
                                    return v
  Input _ -> return Nothing
  InputNull _ -> return Nothing
  RawPtr -> return Nothing
  String -> return Nothing
  where size _ (ConstSize n) = return $ n
        size args (Count k n) = let SmallVal v = args !! n in return $ k * (fromIntegral (v .&. 0xFFFFFFFF))
        size args (Arg n) = let SmallVal v = args !! n in return $ fromIntegral (v .&. 0xFFFFFFFF)
        size args (ArgMan n) = let SmallVal v = args !! n in do
                                  z <- fmap (.&. 0xFFFFFFFF) $ tracePeek $ rawTracePtr $ wordPtrToPtr $ fromIntegral v
                                  liftIO $ putStrLn $ "Managed size: " ++ (show z)
                                  return z


readArg :: [Word64] -> Word64 -> ArgType -> Trace SysReqArg
readArg args arg ty = case ty of
  Small -> return $ SmallVal arg
  SmallSize -> return $ SmallVal arg
  Storage _ -> return $ SmallVal arg
  StorageReccomend _ -> return $ SmallVal arg
  MaybeStorage _ -> return $ SmallVal arg
  InOut sz -> readArg args arg (Input sz)
  Input sz -> do sz' <- size args sz
                 fmap Buf $ readByteString (wordTrace arg) sz'
  InputNull sz -> do sz' <- size args sz
                     error "Nopped out because traceReadNullTerm is bugged"  -- fmap Buf $ traceReadNullTerm (wordTrace arg) sz'
  RawPtr -> return $ SmallVal arg
  String -> fmap Buf $ traceReadNullTerm (wordTrace arg) 4096
  Strings -> do p <- tracePeek (wordTrace arg)
                if p == 0
                   then return $ Bufs []
                   else do Buf v  <- readArg args p String
                           Bufs vs <- readArg args (arg + 8) Strings
                           return $ Bufs $ v : vs
  where size _ (ConstSize n) = return $ n
        size args (Arg n) = return $ fromIntegral $ (args !! n) .&. 0xFFFFFFFF
        size args (Count k n) = return $ k * (fromIntegral $ (args !! n) .&. 0xFFFFFFFF)
        size args (ArgMan n) = do z <- fmap (.&. 0xFFFFFFFF) $ tracePeek $ rawTracePtr $ wordPtrToPtr $ fromIntegral $ (args !! n)
                                  liftIO $ putStrLn $ "Managed size: " ++ (show z)
                                  return z

wordTrace :: Word64 -> TracePtr a
wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral
