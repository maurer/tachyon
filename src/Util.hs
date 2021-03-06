module Util where

import Data.Bits

bitCast :: (Bits a, Bits b) => a -> b
bitCast x = foldl (.|.) zeroBits $ map (bit . fst) $ filter snd $ map (\z -> (z, testBit x z)) [0..bitSize x - 1]
