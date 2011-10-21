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

readOutput :: SysReq -> Trace Syscall
readOutput req@(SysReq i args) = do
  regs <- getRegs
  let ret = rax regs
  ress <- zipWithM (readRes ret args) args (getSig i)
  liftIO $ putStrLn "All arguments read."
  return $ Syscall req (SysRes ret ress)

writeOutput :: SysReq -> SysRes -> Trace ()
writeOutput (SysReq i args) (SysRes r ress) = do
  regs <- getRegs
  setRegs $ regs {rax = r}
  -- Note, StorageReccomend is only legal for calls which get passed through
  zipWith3M_ (writeRes $ rax regs) args (getSig i) ress
  where zipWith3M_ f a b c = sequence_ $ zipWith3 f a b c

writeRes :: Word64 -> SysReqArg -> ArgType -> Maybe BS.ByteString -> Trace ()
writeRes ret (SmallVal addr) (StorageReccomend _) (Just bs) =
  if (addr == 0)
    then writeRes ret (SmallVal ret) (Storage undefined) (Just bs)
    else writeRes ret (SmallVal addr) (Storage undefined) (Just bs)
writeRes _ (SmallVal addr) _ (Just bs) =
  writeByteString bs (wordTrace addr)
writeRes _ _ _ _ = return ()

readRes :: Word64 -> [SysReqArg] -> SysReqArg -> ArgType -> Trace (Maybe BS.ByteString)
readRes ret args arg ty = do
  liftIO $ putStrLn $ "Reading: " ++ (show ty)
  readRes' ret args arg ty
readRes' ret args arg ty = case ty of
  Small -> return Nothing
  SmallSize -> return Nothing
  Storage sz -> let SmallVal p = arg
                in do sz' <- size args sz
                      fmap Just $ readByteString (wordTrace p) sz'
  MaybeStorage sz -> let SmallVal p = arg
                     in if (p == 0) then return Nothing else readRes ret args arg (Storage sz)
  StorageReccomend sz -> if arg == SmallVal 0
                           then if (ret > maxBound - 1024)
                                   then return Nothing
                                   else readRes ret args (SmallVal ret) (Storage sz)
                           else readRes ret args arg (Storage sz)
  Input _ -> return Nothing
  InputNull _ -> return Nothing
  RawPtr -> return Nothing
  String -> return Nothing
  where size _ (ConstSize n) = return $ n
        size args (Count k n) = let SmallVal v = args !! n in return $ k * (fromIntegral (v .&. 0xFFFFFFFF))
        size args (Arg n) = let SmallVal v = args !! n in return $ fromIntegral v
        size args (ArgMan n) = let SmallVal v = args !! n in fmap (.&. 0xFFFFFFFF) $ tracePeek $ rawTracePtr $ wordPtrToPtr $ fromIntegral v


readArg :: [Word64] -> Word64 -> ArgType -> Trace SysReqArg
readArg args arg ty = case ty of
  Small -> return $ SmallVal arg
  SmallSize -> return $ SmallVal arg
  Storage _ -> return $ SmallVal arg
  StorageReccomend _ -> return $ SmallVal arg
  MaybeStorage _ -> return $ SmallVal arg
  Input sz -> do sz' <- size args sz
                 fmap Buf $ readByteString (wordTrace arg) sz'
  InputNull sz -> do sz' <- size args sz
                     fmap Buf $ traceReadNullTerm (wordTrace arg) sz'
  RawPtr -> return $ SmallVal arg
  String -> fmap Buf $ traceReadNullTerm (wordTrace arg) 4096
  Strings -> do p <- tracePeek (wordTrace arg)
                if p == 0
                   then return $ Bufs []
                   else do Buf v  <- readArg args p String
                           Bufs vs <- readArg args (arg + 8) Strings
                           return $ Bufs $ v : vs
  where size _ (ConstSize n) = return $ n
        size args (Arg n) = return $ fromIntegral $ (args !! n)
        size args (Count k n) = return $ k * (fromIntegral $ (args !! n) .&. 0xFFFFFFFF)
        size args (ArgMan n) = fmap (.&. 0xFFFFFFFF) $ tracePeek $ rawTracePtr $ wordPtrToPtr $ fromIntegral $ (args !! n)

wordTrace :: Word64 -> TracePtr a
wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral
