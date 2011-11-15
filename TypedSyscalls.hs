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
--  liftIO $ print sid
  let SysSig rsig sigs = getSig sid
  let args = map ($ regs) callConv
  let r = (take (length sigs) args, sigs, rax regs, rsig, sid)
  return r

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
  where mask n n' = n' .&. (foldl setBit 0 [0..(min ((wordSize * 8) - 1) ((n * 8) - 1))])

readBound :: [Word64] -> [Type] -> Bound -> Lookup -> Trace Int
readBound args tys bound self = do
  b <- readBound' args tys bound self
--  liftIO $ putStrLn $ "Got bound: " ++ (show b) ++ " from " ++ (show bound)
  return b
readBound' args tys bound self =
  case bound of
    Const n       -> return n
    Mult bound' m -> do b <- readBound' args tys bound' self
                        return $ m * b
    Lookup l      -> fmap (fromIntegral . ff) $ readLookup args tys l self
  where ff (x, _, _) = x
readLookup :: [Word64] -> [Type] -> Lookup -> Lookup -> Trace (Word64, Word64, Type)
readLookup args tys l s = do
  r@(x, _, ty) <- readLookup' args tys l s
--  liftIO $ putStrLn $ "Got " ++ (show (x, ty)) ++ " from " ++ (show l)
  return r
readLookup' args tys l s =
  case l of
    Arg n      -> assert (n < length args) $ return (args !! n, undefined, tys !! n)
    Index n l' -> do (v, _, t) <- readLookup args tys l' s
    --TODO maybe switch to using the address here and make structs be undefined values
                     case t of
                       Struct stys -> indexer (v + fromIntegral (sum $ map size $ take n stys)) (stys !! n)
                       Ptr _ ty b nt -> indexer (v  + fromIntegral ((size ty) * n)) ty
                       _ -> error $ "Unable to index into a " ++ (show t)
    Undo (Index _ l') -> readLookup args tys l' s
    Undo Self -> readLookup args tys (Undo s) s
    Undo l' -> error $ "Attempted to undo " ++ (show l')
    Self -> readLookup args tys s s
   where indexer v' ty =
           case ty of
             Small _     -> fmap (\x -> (x, v', ty)) $ getWord v' (size ty)
             Ptr _ _ _ _ -> fmap (\x -> (x, v', ty)) $ getWord v' (size ty)
             Struct _    -> return (v', v', ty)

size :: Type -> Int
size (Small n)     = n
size (Struct tys)  = sum $ map size tys
size (Ptr _ _ _ _) = wordSize

input In    = True
input InOut = True
input Out   = False

readArg = readRec input input

readRec :: (IOC -> Bool) -> (IOC -> Bool) -> [Word64] -> [Type] -> Lookup -> Word64 -> Type -> Trace [(Lookup, Datum)]
readRec recurse record args tys look arg ty = do
  case ty of
    Small n -> assert (n <= wordSize) $ do
      return [(look, SmallDatum arg)]
    --Don't bother figuring anything out if we have a null pointer
    Ptr _ _ _ _ | arg == 0 -> return []
    --This is an optimization case to deal with huge buffers
    Ptr ioc ty'@(Small n) bound nt | record ioc -> do b <- fmap (n *) $ readBound args tys bound look
                                                      buf <- case nt of
                                                               UT -> readByteString (raw arg) b
                                                               NT -> traceReadNullTerm (raw arg) b
                                                      return [(look, Buf buf)]
    --This case is meant to catch all direct struct pointers. If they're in arrays of some sort, they'll fall through
    --and recurse back to here later after some checks and such.
    Ptr ioc (Struct stys) (Const (-1)) UT -> fmap concat $ zipWithM (sHelp id ioc) stys $ offs stys
    Ptr ioc (Struct stys) (Const 1) UT -> fmap concat $ zipWithM (sHelp (Index 0) ioc) stys $ offs stys
    p@(Ptr ioc ty' bound t) -> let r = case ty' of
                                         Small _ -> record ioc
                                         _ -> True in
                               if (recurse ioc) && r
                                  then assert ((size ty') <= wordSize) $ do
                                         b <- readBound args tys bound look
                                         tHelp p b 0
                                  else return []
  where tHelp p@(Ptr ioc ty' _ t) b i = do
          let sz = size ty'
          let addr = arg + fromIntegral (i * sz)
          w <- getWord addr sz
          if (w == 0) && (t == NT)
             then return [(Index i look, SmallDatum 0)]
             else do if i < b
                        then do rest <- tHelp p b (i + 1)
                                let (tty, targ) = case ty' of
                                                     Struct _ -> (Ptr ioc ty' (Const (-1)) UT, addr)
                                                     _        -> (ty', w)
                                this <- readRec recurse record args tys (Index i look) targ tty
                                return $ this ++ rest
                        else return []
        offs stys = offs' (0, 0) stys
        offs' n@(i,k) (sty:stys) = n : (offs' (i + 1, k + (fromIntegral $ size sty)) stys)
        sHelp k ioc ty (i, off)  =
          case ty of
            Small n -> if record ioc
                          then do w <- getWord (arg + off) n
                                  readRec recurse record args tys (Index i (k look)) w ty
                          else return []
            Ptr _ _ _ _ -> if recurse ioc
                              then do w <- getWord (arg + off) wordSize
                                      readRec recurse record args tys (Index i (k look)) w ty
                              else return []
            Struct _ -> error "Nested structs disallowed"

--  where outAssert ty' bound nt x = assert (finalLevel ty') $ assert (bound /= Unbounded) $ assert (nt /= NT) $ x
        

getSig :: SyscallID -> SysSig
getSig sid =
  case Map.lookup sid syscallTable of
    Just s -> s
    Nothing  -> error $ "Unimplemented syscall: " ++ (show sid)

readOutput :: Trace SysRes
readOutput = do
  (args, sigs, ret, rsig, _) <- setup
--  liftIO $ print sigs
  ress <- fmap concat $ zipWithM3 (readRes (args ++ [ret]) (sigs ++ [rsig])) (map Arg [0..]) (args ++ [ret]) (sigs ++ [rsig])
  return $ SysRes ret ress

inout _ = True
output Out = True
output InOut = True
output _ = False

readRes :: [Word64] -> [Type] -> Lookup -> Word64 -> Type -> Trace [(Lookup, Datum)]
readRes = readRec inout output

writeOutput :: SysRes -> Trace ()
writeOutput (SysRes r ress) = do
  regs <- getRegs
  --liftIO $ print ress
  (args, sigs, _, rsig, _) <- setup
--  liftIO $ print sigs
  setRegs $ regs {rax = r}
  mapM_ (writeRes (args ++ [r]) (sigs ++ [rsig])) ress

wordTrace :: Word64 -> TracePtr a
wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

writeRes :: [Word64] -> [Type] -> (Lookup, Datum) -> Trace ()
writeRes args types (look, Buf x) = do
--  liftIO $ putStrLn $ "Trying to output buffer at " ++ (show look)
  (v, _, ty) <- readLookup args types look (error "Self shouldn't be in our output")
  case ty of
     Ptr _ _ _ _ -> return ()
     _ -> error $ "Not a pointer! " ++ (show ty) ++ " " ++ (show look)
  writeByteString x (raw v)
--  liftIO $ putStrLn "Success."
--Arguments are recorded at the moment (maybe should stop this) but no reason to replay them.
writeRes _ _ (Arg _, _) = return ()
writeRes args types (look, SmallDatum x) = do
--  liftIO $ putStrLn $ "Trying to output small datum at " ++ (show look)
  (_, v, Small n) <- readLookup args types look (error "No self :(")
  case n of
    1 -> tracePoke (raw v) ((fromIntegral x) :: Word8)
    2 -> tracePoke (raw v) ((fromIntegral x) :: Word16)
    4 -> tracePoke (raw v) ((fromIntegral x) :: Word32)
    8 -> tracePoke (raw v) ((fromIntegral x) :: Word64)
    _ -> error "Unsupported smallsize"
--  liftIO $ putStrLn "Success."
