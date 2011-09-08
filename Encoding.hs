module Encoding where
import Syscall
import Data.Binary
import qualified Data.ByteString as BS
import Data.Int
instance Binary SyscallID where
   put x = put $ fromEnum x
   get = fmap toEnum get

instance Binary Syscall where
   put (UnknownSyscall n) = do put (0 :: Word8)
                               put n
   put (Syscall n p o) = do put (1 :: Word8)
                            put n
                            put p
                            put o
   get = do (x :: Word8) <- get
            case x of
               0 -> do n <- get
                       return $ UnknownSyscall n
               1 -> do n <- get
                       p <- get
                       o <- get
                       return $ Syscall n p o
instance Binary SysOutput where
   put (SysOut n x) = do
      put (0 :: Word8)
      put n
      put x
   put (UnknownSysOut) = put (1 :: Word8)
   get = do
      (c :: Word8) <- get
      case c of
         0 -> do n <- get
                 x <- get
                 return (SysOut n x)
         1 -> return UnknownSysOut

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

instance Binary SysParam where
   put (SysString x) = do put (0 :: Word8)
                          put x
   put (SysStrings x) = do put (1 :: Word8)
                           put x
   put (SysAddr n) = do put (2 :: Word8)
                        put n
   put (SysSmall n) = do put (3 :: Word8)
                         put n
   put (SysSmallSize n) = do put (4 :: Word8)
                             put n
   get = do (c :: Word8) <- get
            case c of
               0 -> do x <- get
                       return $ SysString x
               1 -> do x <- get
                       return $ SysStrings x
               2 -> do x <- get
                       return $ SysAddr x
               3 -> do x <- get
                       return $ SysSmall x
               4 -> do x <- get
                       return $ SysSmallSize x
type NullString = [NullHelp]
