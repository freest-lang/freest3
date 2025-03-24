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

data MessageType = Simple   T.Type B.ByteString
                 | Str      B.ByteString
                 | LLabel   B.ByteString Word8 B.ByteString
                 | Finish
                 | Error    String
    deriving Show


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
        6 -> getFromLabelMessage hdl  >>= \(choiceId, label, originalMessage) -> return $ LLabel choiceId label originalMessage
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

            getFromLabelMessage :: Handle -> IO (B.ByteString, Word8, B.ByteString)
            getFromLabelMessage hdl = do
                lenBytes <- B.hGetNonBlocking hdl 4
                let length = B.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 lenBytes 
                labelList <- B.hGetNonBlocking hdl length
                label <- B.hGetNonBlocking hdl 1
                return (labelList, B.head label, B.concat [tagB, lenBytes, labelList, label])

{-
    Receives the rest of the message. The rest of the message depends on the type received

    First parameter  -> The handle to receive the rest of the message
    Second parameter -> The first byte of the message. (Could be changed to the type instead of the byte 
                        to improve the readability of the code)
    Return           -> The rest of the message
-}
getRestOfMessage :: Handle -> Word8 -> IO B.ByteString
getRestOfMessage hdl n =
    case n of
        2 -> B.hGetNonBlocking hdl 4
        3 -> B.hGetNonBlocking hdl 4
        4 -> B.hGetNonBlocking hdl 1
        7 -> return B.empty
        _ -> return B.empty

{- 
    Checks the type receive with the type in the state machine. If the type is was invalid the error
    message is returned. If the type is valid it will see if the type receive is the end type. 
    If it is the end type it will return Ended, the rest can be ignored. If it is not the end type it 
    will return Valid with the rest of the type of the state machine.

    First parameter  -> Type received 
    Second parameter -> The current state of the state machine
    Return           -> (Result, T.Type) where Result is the result of the comparison and 
                        T.Type is the rest of the state machine
-}
-- checkAgainstSM :: T.Type -> T.Type -> (Result, T.Type)
-- checkAgainstSM t sm = case compareTypes n_sm t Nothing of
--     Left errorMsg -> (Invalid errorMsg, T.Skip defaultSpan)
--     Right rest -> do 
--         if itEnded t then (Ended, rest) else (Valid, rest)
--     where
--         n_sm = normalise sm
--         itEnded (T.End _ _) = True
--         itEnded _ = False

checkAgainstSM :: T.Type -> T.Type -> T.Polarity -> (Result, T.Type)
checkAgainstSM t sm p = 
    let typeOfMessage = FSM.Normal t p in
    case FSM.check typeOfMessage sm of
        Left errorMsg -> (Invalid errorMsg, T.Skip defaultSpan)
        Right rest -> do 
            if itEnded t then (Ended, rest) else (Valid, rest)
    where
        itEnded (T.End _ _) = True
        itEnded _ = False

{-
    Handles the messages received. First it's going to get the current state of the state machine and it will check if the
    tag is recognised and get the correspondent type (Tag is the first byte of the message). If it is not recognised it will
    return Invalid with the error message. If it is recognised it will get the rest of the message and then check if the type is
    valid. Then it will see the possible outcomes. If the type is valid it will return Valid with the rest of the message. If the
    type is the end type it will return Ended with the rest of the message (It don't contain nothing relevant).
    If the type is invalid it will return Invalid with the error message. 

    First parameter  -> The handle that send the message
    Second parameter -> The message received
    Third parameter  -> The polarity of the message
    Fourth parameter -> The state machine
    Return           -> (Result, B.ByteString) where Result is the result of the comparison and B.ByteString is
                        the message to be sent to the other side if everything is ok. 
-}

handleMessages' :: Handle -> B.ByteString -> T.Polarity -> T.View -> StateMachine -> IO (Result, B.ByteString)
handleMessages' hdl msg p v sm = do
    sm' <- takeMVar sm
    result <- extractMessage hdl msg
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
                    return (Ended, msg)
        Error errorMsg -> do
            putStrLn errorMsg
            return (Invalid errorMsg, B.empty)
        LLabel choiceId label msg -> do
            let choiceId' = B.take (B.length choiceId - 1) choiceId -- remove the \0
            let b = BC.split '|' choiceId' 
            let listOfVariables = map (\s -> Variable defaultSpan (BC.unpack s) (-1)) b
            let variable = Variable defaultSpan (BC.unpack (b !! fromIntegral label)) (-1)
            case FSM.check (FSM.Label listOfVariables variable v) sm' of
                Left errorMsg -> do
                    putStrLn errorMsg
                    return (Invalid errorMsg, B.empty)
                Right rest -> do
                    putMVar sm rest
                    return (Valid, msg)

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
            (False, _)     -> handleMessages' hdl1 h1Tag T.Out T.Internal sm >>= handleResult hdl2
            (_, False)     -> handleMessages' hdl2 h2Tag T.In T.External sm >>= handleResult hdl1
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
