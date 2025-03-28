module Monitor (main) where
import Network.Socket
import qualified Data.List.NonEmpty as NE
import Control.Concurrent
import Network.Socket.ByteString (recv)
import Typing.Normalisation (normalise)
import qualified Data.ByteString as B
import Data.Word (Word8, Word32)
import Syntax.Base (defaultSpan, Variable(..))
import qualified Syntax.Type as T
import Parse.Parser (parseType)
import qualified FSM
import System.IO
import System.Posix.Types
import Foreign.C (getErrno)
import qualified Data.ByteString.Char8 as BC 
import Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString.Lazy as LBS
import Data.Bits (shiftL, (.|.))
import System.Environment (getArgs)
{-
    Result represents the result of the comparison between the type received and the type in the state machine.
    Valid   -> The type received was valid 
    Ended   -> The type received was the end type
    Invalid -> The type received was invalid. It contains the error message to be sent to both parties
-}
data Result = Valid
            | Ended
            | Invalid String

{-
    Representation of the several type of messages that can be received. The Int, Float, Char and Bool are simple types
    that don't need any extra information. The Str type is a string that needs to be received with the length of the string.
    The LLabel type is a label that needs to be received with the length of the label identification, the label identification 
    and the label itself. The Finish type helps to identify the end of the communication. The Error type is used to identify
    an error message that needs to be sent to both parties.
-}

data MessageType = Simple   T.Type B.ByteString
                 | Str      B.ByteString
                 | LLabel   String B.ByteString
                 | Finish
                 | Error    String
    deriving Show

{-
    First it will get the tag of the message. The tag is the first byte of the message. Then it will check the tag and 
    according to the tag it will get the rest of the message. The rest of the message depends on the tag.

    First parameter  -> The handle to receive the rest of the message
    Second parameter -> The tag of the message
    Return           -> The type of the message
-}
extractMessage :: Handle -> B.ByteString -> IO MessageType
extractMessage hdl tagB = 
    let tag = B.head tagB in
    case tag of
        0 -> return $ Simple (T.unit defaultSpan) tagB
        1 -> getRestOfMessage hdl tag >>= \originalMessage -> return $ Simple (T.Var defaultSpan (Variable defaultSpan "Bool" (-1))) originalMessage
        2 -> getRestOfMessage hdl tag >>= \originalMessage -> return $ Simple (T.Int defaultSpan)   originalMessage
        3 -> getRestOfMessage hdl tag >>= \originalMessage -> return $ Simple (T.Float defaultSpan) originalMessage
        4 -> getRestOfMessage hdl tag >>= \originalMessage -> return $ Simple (T.Char defaultSpan)  originalMessage
        5 -> getFromStringMessage hdl >>= \originalMessage -> return $ Str originalMessage
        6 -> getFromLabelMessage hdl  >>= \(label, originalMessage) -> return $ LLabel label originalMessage
        7 -> return Finish
        _ -> return $ Error "The tag was not recognised"
        where
            getRestOfMessage :: Handle -> Word8 -> IO B.ByteString
            getRestOfMessage hdl n =
                case n of
                    1 -> B.hGetNonBlocking hdl 1 >>= \rest -> return $ B.concat [tagB, rest]
                    2 -> B.hGetNonBlocking hdl 4 >>= \rest -> return $ B.concat [tagB, rest]
                    3 -> B.hGetNonBlocking hdl 4 >>= \rest -> return $ B.concat [tagB, rest]
                    4 -> B.hGetNonBlocking hdl 1 >>= \rest -> return $ B.concat [tagB, rest]
                    _ -> return B.empty -- Never going to happen

            getFromStringMessage :: Handle -> IO B.ByteString
            getFromStringMessage hdl = do
                lenBytes <- B.hGetNonBlocking hdl 4
                let length = B.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 lenBytes 
                str <- B.hGetNonBlocking hdl length
                return $ B.concat [tagB, lenBytes, str]

            getFromLabelMessage :: Handle -> IO (String, B.ByteString)
            getFromLabelMessage hdl = do
                lenBytes <- B.hGetNonBlocking hdl 4
                let length = B.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 lenBytes 
                bytes <- B.hGetNonBlocking hdl length
                let label = (BC.unpack . B.init) bytes
                return (label, B.concat [tagB, lenBytes, bytes])

{-
    Handles the messages receive. First it will take the current state of the state machine. Then it will extract the message
    and compare it with the state machine. If the message is valid it will return the message to be sent to the other party.
    If the message is invalid it will return the error message to be sent to both parties. If the message is the end type
    it will close the handle.

    First parameter  -> The handle that send the message
    Second parameter -> The message received (Tag)
    Third parameter  -> The polarity of the message
    Fourth parameter -> The state machine
    Return           -> (Result, B.ByteString) where Result is the result of the comparison and B.ByteString is
                        the message to be sent to the other side if everything is ok. 
-}

handleMessages' :: Handle -> B.ByteString -> T.Polarity -> StateMachine -> IO (Result, B.ByteString)
handleMessages' hdl tag p sm = do
    sm' <- takeMVar sm
    result <- extractMessage hdl tag
    case result of
        Simple t msg -> do
            case FSM.check (FSM.Normal t p) sm' of
                Left errorMsg -> do
                    putStrLn errorMsg
                    return (Invalid errorMsg, B.empty)
                Right rest -> do
                    putMVar sm rest
                    return (Valid, msg)
        Str msg -> do
            case FSM.check (FSM.Normal (T.String defaultSpan) p) sm' of
                Left errorMsg -> do
                    putStrLn errorMsg
                    return (Invalid errorMsg, B.empty)
                Right rest -> do
                    putMVar sm rest
                    return (Valid, msg)
        Finish -> do
            case FSM.check (FSM.Finish (T.End defaultSpan p)) sm' of
                Left errorMsg -> do
                    putStrLn errorMsg
                    return (Invalid errorMsg, B.empty)
                Right _ -> do
                    hClose hdl
                    return (Ended, tag)
        Error errorMsg -> do
            putStrLn errorMsg
            return (Invalid errorMsg, B.empty)
        LLabel label msg -> do
            case FSM.check (FSM.Label (Variable defaultSpan label (-1)) (getView p)) sm' of
                Left errorMsg -> do
                    putStrLn errorMsg
                    return (Invalid errorMsg, B.empty)
                Right rest -> do
                    putMVar sm rest
                    return (Valid, msg)
    where 
        getView :: T.Polarity -> T.View
        getView T.Out = T.Internal
        getView T.In = T.External

{-
    Encodes a string with the length of the string. The length is encoded as a 4-byte big-endian ByteString. 
    The string is encoded as a ByteString. The tag is a single byte that represents the type of the message.

    First parameter  -> The string to be encoded
    Return           -> The encoded string
-}
encodeStringWithLength :: String -> B.ByteString
encodeStringWithLength str = 
    let strLength = fromIntegral (length str + 1) :: Word32
        -- Encode the length as 4-byte big-endian ByteString
        lengthBytes = LBS.toStrict $ runPut (putWord32be strLength)
        -- Convert the string to ByteString
        strBytes = BC.pack str
        tagByte = B.singleton 8 -- Tag for error message
        nullTerminator = B.singleton 0 -- Add null terminator
    in
        -- Concatenate the length bytes and string bytes
        B.concat[tagByte, lengthBytes, strBytes, nullTerminator]

{-
    Creates a socket and binds it to the host and port. Then it listens for a connection and accepts it. 
    It returns the connection socket.

    First parameter  -> The host to bind the socket
    Second parameter -> The port to bind the socket
    Return           -> The connection socket
-}
createSocket :: HostName -> ServiceName -> IO Socket
createSocket host port = withSocketsDo $ do
    let hints = defaultHints { addrFlags = [], addrSocketType = Stream }
    addr <- NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    putStrLn $ "Listening at port " ++ show port
    (conn1, _) <- accept sock
    return conn1

{-
    Connects to a host and port. It returns the connection socket.

    First parameter  -> The host to connect
    Second parameter -> The port to connect
    Return           -> The connection socket
-}
connectTo :: HostName -> ServiceName -> IO Socket
connectTo host port = withSocketsDo $ do
    let hints = defaultHints { addrFlags = [], addrSocketType = Stream }
    addr <- NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

{-
    It represents what a state machine is. It is a mutable variable that contains a type.
-}
type StateMachine = MVar T.Type

{-
    It receives messages from two handles. It will loop until it receives a message from one of the handles. 
    If it receives a message from one of the handles it will handle the message and then loop again. 
    If it doesn't receive any message it will loop again after 1 second. 
    When the message is receive it will handle the message and then send the message to the other handle. But
    depending of what happened it can close the handle if the message was invalid or the end type.

    Note: It is non-blocking. But it uses polling. Needs to be changed to be done with the register of events

    First parameter  -> The handle to receive messages from the first party
    Second parameter -> The handle to receive messages from the second party
    Third parameter  -> The state machine
-}
nonBlockingReceive :: Handle -> Handle -> StateMachine -> IO ()
nonBlockingReceive hdl1 hdl2 sm = loop
  where
    loop = do
        h1Tag <- B.hGetNonBlocking hdl1 1
        h2Tag <- B.hGetNonBlocking hdl2 1

        case (B.null h1Tag, B.null h2Tag) of
            (True, True)   -> threadDelay 100000 >> putStrLn "Looping" >> loop
            (False, _)     -> handleMessages' hdl1 h1Tag T.Out sm >>= handleResult hdl2
            (_, False)     -> handleMessages' hdl2 h2Tag T.In  sm >>= handleResult hdl1
    handleResult :: Handle -> (Result, B.ByteString)  -> IO ()
    handleResult hdl (Valid, msg) = B.hPut hdl msg >> loop
    handleResult hdl (Ended, msg) = B.hPut hdl msg >> hClose hdl
    handleResult _ (Invalid msg, _) = do 
        let bytes = encodeStringWithLength msg
        B.hPut hdl1 bytes
        B.hPut hdl2 bytes
        hClose hdl1
        hClose hdl2

{-
    Receives the address of the host and port from a socket

    First parameter  -> The socket to receive the address
    Return           -> The host and port
-}
receiveAddress :: Socket -> IO(HostName, ServiceName)
receiveAddress sock = do
    recv sock 1 >>= \addressLen -> do
        let len = fromIntegral $ B.head addressLen
        recv sock len >>= \msg -> do
            let (host, port) = BC.break (== ':') msg
            return (BC.unpack host, BC.unpack $ BC.tail port)

main :: IO ()
main = do
    args <- getArgs
    session_type <- readFile $ head args
    case parseType "" session_type of
        Left e -> print e
        Right t -> do
            let nt = normalise t
            state_machine <- newMVar nt
            sock1 <- createSocket "127.0.0.1" "8080"
            (host, port) <- receiveAddress sock1

            sock2 <- connectTo host port

            hdl1 <- socketToHandle sock1 ReadWriteMode
            hSetBuffering hdl1 NoBuffering

            hdl2 <- socketToHandle sock2 ReadWriteMode
            hSetBuffering hdl2 NoBuffering
            
            if (args !! 1) == "-client" then
                nonBlockingReceive hdl1 hdl2 state_machine
            else 
                nonBlockingReceive hdl2 hdl1 state_machine

            putStrLn "Ended"
