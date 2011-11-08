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
import Control.Exception

callConv :: [Regs -> Word64]
callConv = [rdi, rsi, rdx, r10, r8, r9]

setup :: Trace ([Word64], [Type], Word64, Type, SyscallID)
setup = do
  regs <- getRegs
  let sid = syscallID regs
  let SysSig rsig sigs = getSig sid
  let args = map ($ regs) callConv
  return (args, sigs, rax regs, rsig, sid)

zipWithM3 f x y z = sequence $ zipWith3 f x y z

wordSize = 8

readInput :: Trace SysReq
readInput = do
  (args, sigs, _, _, sid) <- setup
  fmap ((SysReq sid) . concat) $ zipWithM3 (readArg args sigs) (map Arg [0..]) args sigs

nonStruct (Struct _) = False
nonStruct _ = True

raw :: Word64 -> TracePtr a
raw = rawTracePtr . wordPtrToPtr . fromIntegral

getWord :: Word64 -> Int -> Trace Word64
getWord p n = fmap (mask n) $ tracePeek $ raw p
  where mask n n' = n' .&. (foldl setBit 0 [0..((n * 8) - 1)])

readBound :: [Word64] -> [Type] -> Bound -> Lookup -> Trace Int
readBound args tys bound self =
  case bound of
    Unbounded     -> return maxBound
    Const n       -> return n
    Mult bound' m -> do b <- readBound args tys bound' self
                        return $ m * b
    Lookup l      -> fmap (fromIntegral . fst) $ readLookup args tys l self

readLookup :: [Word64] -> [Type] -> Lookup -> Lookup -> Trace (Word64, Type)
readLookup args tys l s =
  case l of
    Arg n      -> assert (n < length args) $ return (args !! n, tys !! n)
    Index n l' -> do (v, t) <- readLookup args tys l' s
                     case t of
                       Struct stys -> indexer (v + fromIntegral (sum $ map size $ take n stys)) (stys !! n)
                       Ptr _ ty b nt -> indexer (v  + fromIntegral ((size ty) * n)) ty
                       _ -> error $ "Unable to index into a " ++ (show t)
    Undo (Index _ l') -> readLookup args tys l' s
    Undo l' -> error $ "Attempted to undo " ++ (show l')
    Self -> readLookup args tys s s
   where indexer v' ty =
           case ty of
             Small _     -> fmap (\x -> (x, ty)) $ getWord v' (size ty)
             Ptr _ _ _ _ -> fmap (\x -> (x, ty)) $ getWord v' (size ty)
             Struct _    -> return (v', ty)

size :: Type -> Int
size (Small n)     = n
size (Struct tys)  = sum $ map size tys
size (Ptr _ _ _ _) = wordSize

readArg :: [Word64] -> [Type] -> Lookup -> Word64 -> Type -> Trace [(Lookup, Datum)]
readArg args tys look arg ty = do
  case ty of
    Small n -> assert (n <= wordSize) $ do
      return [(look, SmallDatum arg)]
    Ptr Out _ _ _ -> return []
    Ptr _ ty' bound NT -> assert ((size ty') <= wordSize) $ assert (nonStruct ty') $ do
      b <- readBound args tys bound look
      ntHelp ty' b 0
  where ntHelp ty' b i = do
          let sz = size ty'
          w <- getWord (arg + fromIntegral (i * sz)) sz
          if w == 0
             then return [(Index i look, SmallDatum 0)]
             else do rest <- if i < b
                                then ntHelp ty' b (i + 1)
                                else return []
                     this <- readArg args tys (Index i look) w ty'
                     return $ this ++ rest

         
--  where outAssert ty' bound nt x = assert (finalLevel ty') $ assert (bound /= Unbounded) $ assert (nt /= NT) $ x
        

getSig :: SyscallID -> SysSig
getSig sid =
  case Map.lookup sid syscallTable of
    Just s -> s
    Nothing  -> error $ "Unimplemented syscall: " ++ (show sid)

readOutput :: Trace SysRes
readOutput = do
  (args, sigs, ret, rsig, _) <- setup
  ress <- fmap concat $ zipWithM (readRes ret args sigs) (args ++ [ret]) (sigs ++ [rsig])
  liftIO $ putStrLn "All arguments read."
  return $ SysRes ret ress

readRes :: Word64 -> [Word64] -> [Type] -> Word64 -> Type -> Trace [(Lookup, Datum)]
readRes = undefined

writeOutput :: SysRes -> Trace ()
writeOutput (SysRes r ress) = do
  regs <- getRegs
  (args, sigs, _, rsig, _) <- setup
  setRegs $ regs {rax = r}
  mapM_ (writeRes (args ++ [r]) (sigs ++ [rsig])) ress

wordTrace :: Word64 -> TracePtr a
wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

writeRes :: [Word64] -> [Type] -> (Lookup, Datum) -> Trace ()
writeRes = undefined
