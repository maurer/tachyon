import System.Trace
import Data.Map (toList)
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Environment
import Numeric
import Data.List
import Foreign.Ptr
import System.Exit

main = do
  coreNames <- getArgs
  cores <- mapM ((fmap toList) . decodeFile) coreNames
  diffs <- mapM mrdiff $ transpose cores
  mapM_ putStrLn diffs
  if and $ map (== "Zone matched") diffs
     then exitWith ExitSuccess
     else exitWith (ExitFailure 1)

instance Binary WordPtr where
  get = fmap fromIntegral $ (get :: Get Word64)
  put x = put $ ((fromIntegral x) :: Word64)


mrdiff :: [(WordPtr, ByteString)] -> IO String
mrdiff [(b0, r0), (b1, r1)] | b0 /= b1 = return $ "Mismatched start addresses: " ++ (show (b0, b1))
                            | r0 /= r1 = do let target = showHex b0 ""
                                            BS.writeFile (target ++ ".A") r0
                                            BS.writeFile (target ++ ".B") r1
                                            return $ "Mismatched data payload@" ++ (showHex b0 "")
                            | otherwise = return $ "Zone matched"
