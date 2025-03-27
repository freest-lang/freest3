{-# LANGUAGE InstanceSigs #-}
module Interpreter.Test (main) where

import           Interpreter.Value
import qualified Network.Socket as NS
import           Control.Concurrent
import           System.IO
import qualified Network.Socket.ByteString as NSB
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC 
import qualified Data.Binary as Bin
import           Data.Word (Word32, Word8)
import           GHC.Float (castFloatToWord32, double2Float, float2Double, castWord32ToFloat)
import Control.Monad (void)

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

class Serializable a where
    serialize :: a -> B.ByteString
    deserialize :: Word8 -> HalfChannel -> IO (a, HalfChannel)

instance Serializable Value where
    serialize :: Value -> BC.ByteString
    serialize Unit        = B.singleton 0
    -- serialize (Character c) = B.singleton 1 <> B.singleton (fromIntegral (fromEnum c))
    serialize (Integer i)   = B.singleton 2 <> toStrict1 (Bin.encode (fromIntegral i :: Word32))
    serialize (Float   f)   = B.singleton 3 <> toStrict1 (Bin.encode (castFloatToWord32 (double2Float f)))
    serialize (Character c) = B.singleton 4 <> BC.singleton c
    serialize (String  s)   =
        let len = fromIntegral (1 + length s) :: Word32 
        in B.singleton 5 <> toStrict1 (Bin.encode len) <> BC.pack s <> B.singleton 0 -- added the null terminator
    serialize _ = error "Not implemented"

    deserialize :: Word8 -> HalfChannel -> IO (Value, HalfChannel)
    deserialize 0 hc = return (Unit, hc)
    deserialize 2 hc = do
        v <- fmap BL.fromStrict (NSB.recv hc 4)
        return (Integer $ fromIntegral (Bin.decode v :: Word32), hc)
    deserialize 3 hc = do
        f <- fmap BL.fromStrict (NSB.recv hc 4)
        return (Float $ float2Double . castWord32ToFloat $ (Bin.decode f :: Word32), hc)
    deserialize 4 hc = do
        c <- NSB.recv hc 1
        return (Character $ BC.head c , hc)
    deserialize 5 hc = do
        lenBytes <- fmap BL.fromStrict (NSB.recv hc 4)
        let len = fromIntegral (Bin.decode lenBytes :: Word32)
        s <- NSB.recv hc len
        return (String $ BC.unpack (B.init s), hc)
    deserialize _ _ = error "Not implemented"
    
type HalfChannel = NS.Socket

send :: Value -> HalfChannel -> IO ()
send v hc = void (NSB.send hc (serialize v)) -- Improve this to catch errors in the socket

receive :: HalfChannel -> IO (Value, HalfChannel)
receive hc = do
    NSB.recv hc 1 >>= \bytes ->
        deserialize (B.head bytes) hc

close :: HalfChannel -> IO Value
close hc = NSB.send hc (B.singleton 7) >> NS.close hc >> return Unit

wait :: HalfChannel -> Value
wait hc = IOValue $ void (NSB.recv hc 1) >> NS.close hc >> return Unit

-- When connect while a client we have to send the address of the server
newHalfChannel :: Either (NS.HostName, NS.ServiceName) ((NS.HostName, NS.ServiceName), String)  -> IO HalfChannel
newHalfChannel (Left (host, port)) = NS.withSocketsDo $ do
    let hints = NS.defaultHints { NS.addrFlags = [], NS.addrSocketType = NS.Stream }
    addr <- NE.head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 1
    putStrLn $ "Listening at port " ++ show port
    (conn1, _) <- NS.accept sock
    return conn1

newHalfChannel (Right ((host, port), sv_addr)) = NS.withSocketsDo $ do
    let hints = NS.defaultHints { NS.addrFlags = [], NS.addrSocketType = NS.Stream }
    addr <- NE.head <$> NS.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.connect sock (NS.addrAddress addr)
    let len = fromIntegral (length sv_addr) :: Word8
    let bytes = toStrict1 (Bin.encode len) <> BC.pack sv_addr
    NSB.send sock bytes
    return sock

main :: IO ()
main = do
    let addr = ("127.0.0.1", "8081")
    hc <- newHalfChannel $ Left addr
    -- hc <- newHalfChannel $ Right (addr, "127.0.0.1:8081")
    receive hc >>= \(v, _) ->
        print v >> send (String "aaaaaaaa") hc
    
    return $ wait hc
    -- send (Integer 42) hc
    -- send (Integer 43) hc
    -- receive hc >>= print . fst
    -- close hc

    print "Done"

    