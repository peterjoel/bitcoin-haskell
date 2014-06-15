
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (foldl')


type ECPoint = (Integer, Integer)

--instance Num ECPoint where
--    (ECPoint x y) + (ECPoint x' y') = ()

-- The proven prime
pcurve = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 -1 

-- Number of points in the field
numPoints = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
-- These two defines the elliptic curve. y^2 = x^3 + Acurve * x + Bcurve
curveA = 0
curveB = 7

-- This is our generator point. Trillions of dif ones possible
gen = ( 55066263022277343669578718895168534326250603453777594175500187360389116729240
      , 32670510020758816978083085130507043184471273380659243275938904335757337482424 
      )
-- Gx / Gy

privKey = 0xA0DC65FFCA799873CBEA0AC274015B9526505DAAAED385155425F7337704883E

modinv :: Integer -> Integer -> Integer
modinv a n = loop 1 (a `mod` n) 0 n
    where 
        loop lm low hm high = 
            if low > 1
                then let ratio = high `div` low
                         nm = hm - lm * ratio
                         new = high - low * ratio
                     in loop nm new lm low
                else lm `mod` n


ecadd :: ECPoint -> ECPoint -> ECPoint
ecadd (ax, ay) (bx, by) = (x, y)
    where lamadd = ((by - ay) * modinv (bx-ax) pcurve) `mod` pcurve
          x = (lamadd * lamadd - ax - bx) `mod` pcurve
          y = (lamadd * (ax- x) - ay) `mod` pcurve

ecdouble :: ECPoint -> ECPoint
ecdouble (ax, ay) = (x, y)
    where lam = ((3 * ax * ax + curveA) * (modinv (2 * ay) pcurve)) `mod` pcurve
          x = (lam * lam - 2 * ax) `mod` pcurve
          y = (lam * (ax - x) - ay) `mod` pcurve

ecmultiply :: ECPoint -> Integer -> ECPoint
ecmultiply gp m 
    | m == 0 = error "Invalid Scalar"
    | m >= numPoints = error "Invalid Private Key"
    | otherwise = foldl' doubleAdd gp scalarBin
        where scalarBin = drop 1 . binaryStr $ m
              doubleAdd q '1' = ecadd (ecdouble q) gp
              doubleAdd q _   = ecdouble q
   



binaryStr :: (Integral n, Show n) => n -> String
binaryStr n = showIntAtBase 2 intToDigit n ""

hexStr :: (Integral n, Show n) => n -> String
hexStr n = showIntAtBase 16 intToDigit n ""

compressKey :: ECPoint -> String
compressKey (x, y) = "02" ++ zfill 64 (hexStr x)
    where zfill len s = if length s < len 
                            then zfill len ('0':s)
                            else s

main = do
        print $ "Private key: " ++ show privKey
        let key = ecmultiply gen privKey
        print $ "Uncompressed public key: " ++ show key
        print $ "Official compressed key: " ++ compressKey key
