{-# LANGUAGE InstanceSigs #-}
module Interpreter.Test (Serializable(..), toStrict1) where

import           Interpreter.Value
import           Syntax.Base (Variable(..), defaultSpan)
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC 
import qualified Data.Binary as Bin
import           Data.Word (Word32, Word8)
import           GHC.Float (castFloatToWord32, double2Float, float2Double, castWord32ToFloat)
import qualified Interpreter.Value as V


toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: Word8 -> V.HalfChannel -> IO a

instance Serializable Value where
    serialize :: Value -> BC.ByteString
    serialize Unit          = B.singleton 0
    serialize (Cons (Variable _ "False" _) []) = B.singleton 1 <> B.singleton 0
    serialize (Cons (Variable _ "True" _) [])  = B.singleton 1 <> B.singleton 1
    serialize (Integer i)   = B.singleton 2 <> toStrict1 (Bin.encode (fromIntegral i :: Word32))
    serialize (Float   f)   = B.singleton 3 <> toStrict1 (Bin.encode (castFloatToWord32 (double2Float f)))
    serialize (Character c) = B.singleton 4 <> BC.singleton c
    serialize (String  s)   =
        let len = fromIntegral (1 + length s) :: Word32 
        in B.singleton 5 <> toStrict1 (Bin.encode len) <> BC.pack s <> B.singleton 0 -- added the null terminator
    serialize _ = error "Not implemented"

    deserialize :: Word8 -> V.HalfChannel -> IO Value
    deserialize 0 hc = return Unit
    deserialize 1 hc = do
        c <- B.head <$> NSB.recv hc 1 
        if c == 0
            then return (Cons (Variable defaultSpan "False" (-1)) [])
            else return (Cons (Variable defaultSpan "True" (-1)) [])
        
    deserialize 2 hc = do
        v <- fmap BL.fromStrict (NSB.recv hc 4)
        return (Integer $ fromIntegral (Bin.decode v :: Word32))
    deserialize 3 hc = do
        f <- fmap BL.fromStrict (NSB.recv hc 4)
        return (Float $ float2Double . castWord32ToFloat $ (Bin.decode f :: Word32))
    deserialize 4 hc = do
        c <- NSB.recv hc 1
        return (Character $ BC.head c)
    deserialize 5 hc = do
        lenBytes <- fmap BL.fromStrict (NSB.recv hc 4)
        let len = fromIntegral (Bin.decode lenBytes :: Word32)
        s <- NSB.recv hc len
        return (String $ BC.unpack (B.init s))
    deserialize _ _ = error "Not implemented"
    
-- main :: IO ()
-- main = do
--     let addr = ("127.0.0.1", "8081")
--     hc <- newHalfChannel $ Left addr
--     -- hc <- newHalfChannel $ Right (addr, "127.0.0.1:8081")
--     receive hc >>= \(v, _) ->
--         print v >> send (Cons (Variable defaultSpan "False" (-1)) []) hc >> wait hc
--     -- send (Integer 42) hc
--     -- send (Integer 43) hc
--     -- receive hc >>= print . fst
--     -- close hc

--     print "Done"

    