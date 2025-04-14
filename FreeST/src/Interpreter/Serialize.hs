{-# LANGUAGE InstanceSigs #-}
module Interpreter.Serialize (Serializable(..), toStrict1) where

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
import Debug.Trace (trace)

import          Control.Monad (void) -- REMOVE
import Data.Fixed (Uni)

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: Word8 -> V.HalfChannel -> IO a

instance Serializable Value where
    serialize :: Value -> BC.ByteString
    serialize d@(Cons _ _) = let bytes = serialize' d in
        let len = fromIntegral (B.length bytes) :: Word32 in
        let lenBytes = toStrict1 (Bin.encode len) in
        B.singleton 0 <> lenBytes <> bytes         
    serialize Unit            = B.singleton 1
    serialize i@(Integer _)   = B.singleton 2 <> serialize' i
    serialize f@(Float   _)   = B.singleton 3 <> serialize' f
    serialize c@(Character _) = B.singleton 4 <> serialize' c
    serialize s@(String  _)   = B.singleton 5 <> serialize' s
    serialize l@(Label  _)    = B.singleton 6 <> serialize' l
    serialize _ = error "Not implemented"

    deserialize :: Word8 -> V.HalfChannel -> IO Value
    deserialize 0 hc = do
        lenBytes <- fmap BL.fromStrict (NSB.recv hc 4)
        let len = fromIntegral (Bin.decode lenBytes :: Word32)
        bytes <- NSB.recv hc len
        return $ processMessage bytes []

    deserialize 1 hc = return Unit
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
    
    deserialize 6 hc = do
        lenBytes <- fmap BL.fromStrict (NSB.recv hc 4)
        let len = fromIntegral (Bin.decode lenBytes :: Word32)
        s <- NSB.recv hc len
        return (Label $ BC.unpack (B.init s))
    
    deserialize _ _ = error "Not implemented"
   

serialize' :: Value -> B.ByteString
serialize' (Cons (Variable _ name _) vs) = 
    let lenArgs = fromIntegral (length vs) :: Word8 in
    let len = fromIntegral (length name) :: Word8 in
    serializeListOfLists vs <> B.singleton 8 <> B.singleton lenArgs <> B.singleton len <> BC.pack name
    where 
        serializeListOfLists :: [[Value]] -> B.ByteString
        serializeListOfLists [] = B.empty
        serializeListOfLists (x:xs) = serializeList x <> serializeListOfLists xs

        serializeList :: [Value] -> B.ByteString
        serializeList [] = B.empty
        serializeList (cons@(Cons _ _): xs) = serialize' cons <> serializeList xs
        serializeList (x:xs) = serialize x <> serializeList xs

serialize' (Integer i)   = toStrict1 (Bin.encode (fromIntegral i :: Word32))
serialize' (Float   f)   = toStrict1 (Bin.encode (castFloatToWord32 (double2Float f)))
serialize' (Character c) = BC.singleton c
serialize' (String  s)   =
    let len = fromIntegral (1 + length s) :: Word32 
    in toStrict1 (Bin.encode len) <> BC.pack s <> B.singleton 0 -- added the null terminator
serialize' (Label  l)    =
    let len = fromIntegral (1 + length l) :: Word32
    in toStrict1 (Bin.encode len) <> BC.pack l <> B.singleton 0 -- added the null terminator
serialize' _ = error "Not implemented"

processMessage :: B.ByteString -> [[Value]] -> Value
processMessage bytes state = 
    case B.uncons bytes of 
        Just (tag, rest) -> getValue tag rest
        Nothing -> head . head $ state
    where
        getValue :: Word8 -> B.ByteString -> Value
        getValue 1 rest = processMessage rest (state ++ [[Unit]])
        getValue 2 rest = 
            let (intBytes, rest') = B.splitAt 4 rest
                intVal = fromIntegral (Bin.decode (BL.fromStrict intBytes) :: Word32)
            in processMessage rest' (state ++ [[Integer intVal]])
        getValue 3 rest =
            let (floatBytes, rest') = B.splitAt 4 rest
                floatVal = float2Double . castWord32ToFloat $ (Bin.decode (BL.fromStrict floatBytes) :: Word32)
            in processMessage rest' (state ++ [[Float floatVal]])
        getValue 4 rest =
            let (charBytes, rest') = B.splitAt 1 rest
                charVal = BC.head charBytes
            in processMessage rest' (state ++ [[Character charVal]])
        getValue 5 rest =
            let (lenBytes, rest') = B.splitAt 4 rest
                len = fromIntegral (Bin.decode (BL.fromStrict lenBytes) :: Word32)
                (strBytes, rest'') = B.splitAt len rest'
                strVal = BC.unpack (B.init strBytes)
            in processMessage rest'' (state ++ [[String strVal]])
        getValue 6 rest =
            let (lenBytes, rest') = B.splitAt 4 rest
                len = fromIntegral (Bin.decode (BL.fromStrict lenBytes) :: Word32)
                (labelBytes, rest'') = B.splitAt len rest'
                labelVal = BC.unpack (B.init labelBytes)
            in processMessage rest'' ([Label labelVal]:state)
        getValue 8 rest =
            let (lenArgsBytes, rest') = B.splitAt 1 rest
                (lenBytes, rest'') = B.splitAt 1 rest'
                (nameBytes, rest''') = B.splitAt (fromIntegral len) rest''
                
                lenArgs = B.head lenArgsBytes
                name = BC.unpack nameBytes
                len = B.head lenBytes
                (stateRest, args) =getArgs (fromIntegral lenArgs) state
            in 
                processMessage rest''' (stateRest ++ [[Cons (Variable defaultSpan name (-1)) args]])

        getValue i v = error "Not implemented"

getArgs :: Int -> [a] -> ([a], [a])
getArgs n xs
  | n <= 0    = (xs, [])
  | n > len  = error "Number of arguments of the constructor is incorrect"
  | otherwise = splitAt (len - n) xs
  where len = length xs