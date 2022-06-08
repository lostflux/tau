
module Ciphers.AffineCipher (
    strtoint
  , intToStr
  , encrypt, encryptAll
  , decrypt, decryptAll
) where


import Data.Char (ord)
import Ciphers.AbstractAlgebra (mulN, addN)

intToStr :: Integral a => a -> String
intToStr int = do
  let s = drop 2 $ bin int
  -- let s' = concat [replicate (8 - (length m `mod` 8)) '0' ++ m | m <- s]
  let s'' = 0
  ""



bin :: (Integral a1) => a1 -> String
bin 0 = "0"
bin n
  | even n    = "0" ++ bin (n `div` 2)
  | otherwise = "1" ++ bin (n `div` 2)

frombin :: String -> Int
frombin "" = 0
frombin ('0':xs) = frombin xs
frombin ('1':xs) = 2 ^ length xs + frombin xs

strtoint :: String -> Int
strtoint = undefined
-- strtoint str = do
--   let modn = [drop 2 $ (bin . ord) c | c <- str] 
--   let modn' = concat [replicate (8 - length m) '0' ++ m | m <- modn]
--   frombin modn'

encrypt :: Integral a => a -> a -> a -> a -> a
encrypt base k1 k2 val = addN base k2 $ mulN base k1 val

encryptAll :: Integral a => a -> a -> a -> [a] -> [a]
encryptAll base k1 k2 = map (encrypt base k1 k2)

decrypt :: Integral a => a -> a -> a -> a -> a
decrypt base subt mult val =
  mulN base (val - subt) mult

decryptAll :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
decryptAll base subt mult = map (decrypt base subt mult)

-- >>> strtoint "A"
-- 304494134

b = 0x10

-- >>> b
-- 16


-- # strtoint is used several time in the challenge; it takes as input a string block and returns as output an integer encoding (ASCII bytes)
-- def strtoint(plaintext_block):
--     modn = [bin(ord(c))[2:] for c in plaintext_block]
--     modn = [ ''.join(['0' for i in range(8-len(m))]) + m for m in modn]
--     return int('0b' + ''.join(modn),2)

-- # inttostr is the inverse of strtoint; it takes as input an integer encoding of a string (ASCII bytes) and returns as output the string
-- def inttostr(int_block):
--     s = bin(int_block)[2:]
--     s = ''.join(['0' for i in range(8-(len(s)%8))]) + s
--     s = ['0b' + s[8*i:8*(i+1)] for i in range(len(s)/8)]
--     return ''.join([chr(int(c,2)) for c in s])
