module Syscall where

import System.Trace
import Data.Int
import Util
import Data.Binary
import Data.Word
import qualified Data.ByteString as BS

data SysReq = SysReq SyscallID [SysReqArg] deriving Show
data SysRes = SysRes Word64 [Maybe BS.ByteString] deriving Show

data Syscall = Syscall SysReq SysRes deriving Show

data SysReqArg = SmallVal Word64
               | Buf BS.ByteString
               | Bufs [BS.ByteString] deriving (Show, Eq)

data SysSig = SysSig [ArgType]

data Size = ConstSize Int -- ^ A size which is always the same
          | Arg Int       -- ^ A size which is determined

data ArgType = Small
          | Storage Size -- ^ A pointer to some storage that can be written
          | StorageReccomend Size -- ^ Like storage, but if null, look to the return value for the value of this pointer
          | Input Size   -- ^ A pointer to an input buffer
          | SmallSize -- ^ Like a small, but identity not checked on comparison
          | InputNull Size
          | String
          | Strings
          | RawPtr

data SyscallID = Read
               | Write
               | Open
               | Close
               | Stat
               | FStat
               | LStat
               | Poll
               | LSeek
               | MMap
               | SafeMMap
               | MProtect
               | RTSigProcMask
               | ExitGroup
               | RTSigAction
               | Brk
               | Access
               | ExecVE
               | GetPID
               | MUnmap
               | SetArchPrCtl
               | GetArchPrCtl
               | SetTIDAddr
               | Futex
               | SetRobustList
               | GetRLimit
               | StatFS
               | IOCtl
               | SchedGetParam
               | SchedSetParam
               | FCntl
               | GetDEnts
               | UName
               | GetTimeOfDay
               | SchedGetScheduler
               | SchedSetScheduler
               | ClockGetTime
                deriving (Ord, Show, Eq, Read, Enum)

syscallReg = orig_rax

syscallID :: Regs -> SyscallID
syscallID regs = case syscallReg regs of
   0   -> Read
   1   -> Write
   2   -> Open
   3   -> Close
   4   -> Stat
   5   -> FStat
   6   -> LStat
   7   -> Poll
   8   -> LSeek
   9   -> case (bitCast $ r8 regs) :: Int32 of
             -1 -> SafeMMap
             _ -> MMap
   10  -> MProtect
   11  -> MUnmap
   12  -> Brk
   13  -> RTSigAction
   14  -> RTSigProcMask
   16  -> IOCtl
   20  -> GetPID
   21  -> Access
   59  -> ExecVE
   63  -> UName
   72  -> FCntl
   78  -> GetDEnts
   96  -> GetTimeOfDay
   97  -> GetRLimit
   137 -> StatFS
   142 -> SchedSetParam
   143 -> SchedGetParam
   144 -> SchedSetScheduler
   145 -> SchedGetScheduler
   158 -> case rdi regs of
             0x1001 -> SetArchPrCtl
             0x1002 -> SetArchPrCtl
             0x1003 -> GetArchPrCtl
             0x1004 -> GetArchPrCtl
   202 -> Futex
   218 -> SetTIDAddr
   229 -> ClockGetTime
   231 -> ExitGroup
   273 -> SetRobustList
   n   -> error $ "Unknown syscall: " ++ (show n)

instance Binary SyscallID where
   put x = put $ fromEnum x
   get = fmap toEnum get

instance Binary Syscall where
   put (Syscall p o) = do put p
                          put o
   get = do p <- get
            o <- get
            return $ Syscall p o

instance Binary SysReq where
  put (SysReq n x) = do put n
                        put x
  get = do n <- get
           x <- get
           return $ SysReq n x

instance Binary SysRes where
   put (SysRes n x) = do
      put n
      put x
   get = do
      n <- get
      x <- get
      return (SysRes n x)

encodeThresh = 5
data NullHelp = Rep Int Word8 | Lit BS.ByteString deriving (Read, Show, Eq)
nullEncode :: BS.ByteString -> NullString
nullEncode ns = reverse $ fst $ foldl fuse ([], []) $ map help $ BS.group ns
      where help x | BS.length x >= encodeThresh = Rep (BS.length x) (BS.head x)
                   | otherwise = Lit x
            fuse (xs, fusing) (Lit toFuse) = (xs, (toFuse : fusing))
            fuse (xs, []) rep = (rep : xs, [])
            fuse (xs, fusing) rep = (rep : (Lit $ BS.concat $ reverse fusing) : xs, [])
nullUnbox :: NullString -> BS.ByteString
nullUnbox x = BS.concat $ map unHelp x
      where unHelp (Rep n v) = BS.replicate n v
            unHelp (Lit s) = s

instance Binary SysReqArg where
   put (Buf x) = do put (0 :: Word8)
                    put x
   put (Bufs x) = do put (1 :: Word8)
                     put x
   put (SmallVal n) = do put (2 :: Word8)
                         put n
   get = do (c :: Word8) <- get
            case c of
               0 -> do x <- get
                       return $ Buf x
               1 -> do x <- get
                       return $ Bufs x
               2 -> do x <- get
                       return $ SmallVal x
type NullString = [NullHelp]
