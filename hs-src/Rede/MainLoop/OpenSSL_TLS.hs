{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings,  DeriveDataTypeable #-}
module Rede.MainLoop.OpenSSL_TLS(
    tlsServeWithALPN
    ,tlsServeWithALPNAndFinishOnRequest
    -- ,tlsServeWithALPNOnce

    ,ConnectionIOError(..)
    ,FinishRequest(..)
    ) where 



import           Control.Monad
import           Control.Concurrent.MVar    
import           Control.Exception
import           Data.Foldable              (foldMap)
import           Data.Typeable              
import           Data.Monoid                ()
import           Foreign
import           Foreign.C

import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Unsafe     as BU

import           Rede.MainLoop.PushPullType
           



data ConnectionIOError = ConnectionIOError String
    deriving (Show, Typeable)


data InterruptibleEither a b = 
    Left_I a 
    |Right_I b 
    |Interrupted


data FinishRequest = FinishRequest


instance Exception ConnectionIOError


-- These names are absolutely improper....
-- Session creator
data Connection_t  
-- Session
data Wired_t

type Connection_Ptr = Ptr Connection_t 
type Wired_Ptr = Ptr Wired_t

-- Actually, this makes a listener for new connections
-- connection_t* make_connection(char* certificate_filename, char* privkey_filename, char* hostname, int portno, 
--     char* protocol_list, int protocol_list_len)
foreign import ccall "make_connection" makeConnection :: 
    CString         -- cert filename
    -> CString      -- privkey_filename
    -> CString      -- hostname
    -> CInt         -- port
    -> Ptr CChar    -- protocol list
    -> CInt         -- protocol list length
    -> IO Connection_Ptr

allOk :: CInt 
allOk = 0 

badHappened :: CInt 
badHappened = 1 

timeoutReached :: CInt 
timeoutReached = 3

-- int wait_for_connection(connection_t* conn, wired_session_t** wired_session);
foreign import ccall "wait_for_connection" waitForConnection :: Connection_Ptr -> CInt -> Ptr Wired_Ptr -> IO CInt 

-- int send_data(wired_session_t* ws, char* buffer, int buffer_size);
foreign import ccall "send_data" sendData :: Wired_Ptr -> Ptr CChar -> CInt -> IO CInt 

-- int recv_data(wired_session_t* ws, char* inbuffer, int buffer_size, int* data_recvd);
foreign import ccall "recv_data" recvData :: Wired_Ptr -> Ptr CChar -> CInt -> Ptr CInt -> IO CInt

-- int get_selected_protocol(wired_session_t* ws){ return ws->protocol_index; }
foreign import ccall "get_selected_protocol" getSelectedProtocol :: Wired_Ptr -> IO CInt

-- void dispose_wired_session(wired_session_t* ws);
foreign import ccall "dispose_wired_session" disposeWiredSession :: Wired_Ptr -> IO ()


useBufferSize :: Int
useBufferSize = 4096


type Protocols = [B.ByteString]


protocolsToWire :: Protocols -> B.ByteString
protocolsToWire protocols =  
    LB.toStrict . BB.toLazyByteString $ 
        foldMap (\ protocol 
                ->  (BB.lazyByteString . LB.fromChunks)
                    [ B.singleton $ fromIntegral $ B.length protocol,
                      protocol 
                    ]
        ) protocols


tlsServeWithALPN :: FilePath 
                 -> FilePath 
                 -> String 
                 -> [(String, Attendant)]
                 -> Int 
                 -> IO ()
tlsServeWithALPN certificate_filename key_filename interface_name attendants interface_port = do 

    let protocols_bs = protocolsToWire $ fmap (\ (s,_) -> pack s) attendants
    withCString certificate_filename $ \ c_certfn -> withCString key_filename $ \ c_keyfn -> withCString interface_name $ \ c_iname -> do 

        connection_ptr <- BU.unsafeUseAsCStringLen protocols_bs $ \ (pchar, len) ->
            makeConnection 
                c_certfn
                c_keyfn
                c_iname
                (fromIntegral interface_port)
                pchar 
                (fromIntegral len)

        forever $ do 
            either_wired_ptr <- alloca $ \ wired_ptr_ptr -> 
                let 
                    tryOnce = do 
                        result_code <- waitForConnection connection_ptr defaultWaitTime wired_ptr_ptr
                        let 
                            r = case result_code of  
                                re  | re == allOk        -> do 
                                        p <- peek wired_ptr_ptr
                                        return $ Right  p
                                    | re == timeoutReached -> tryOnce 
                                    | re == badHappened  -> return $ Left "A wait for connection failed"
                        r 
                in tryOnce
                

            case either_wired_ptr of 

                Left msg -> do 
                    putStrLn $ ".. No connection " ++ msg


                Right wired_ptr -> do 
                    let 
                        pushAction datum = BU.unsafeUseAsCStringLen (LB.toStrict datum) $ \ (pchar, len) -> do 
                            result <- sendData wired_ptr pchar (fromIntegral len)
                            case result of  
                                r | r == allOk           -> return ()
                                  | r == badHappened     -> throwIO $ ConnectionIOError "Could not send data"
                        pullAction = do 
                            allocaBytes useBufferSize $ \ pcharbuffer -> 
                                alloca $ \ data_recvd_ptr -> do 
                                    result <- recvData wired_ptr pcharbuffer (fromIntegral useBufferSize) data_recvd_ptr
                                    recvd_bytes <- case result of 
                                        r | r == allOk       -> peek data_recvd_ptr
                                          | r == badHappened -> throwIO $ ConnectionIOError "Could not receive data"

                                    B.packCStringLen (pcharbuffer, fromIntegral recvd_bytes)

                    use_protocol <- getSelectedProtocol wired_ptr

                    putStrLn $ ".. Using protocol: " ++ (show use_protocol)

                    let 
                        maybe_session_attendant = case fromIntegral use_protocol of 
                            n | (use_protocol >= 0)  -> Just $ snd $ attendants !! n 
                              | otherwise          -> Nothing 

                    case maybe_session_attendant of 

                        Just session_attendant -> 
                            session_attendant pushAction pullAction

                        Nothing ->
                            disposeWiredSession wired_ptr


tlsServeWithALPNAndFinishOnRequest :: FilePath 
                 -> FilePath 
                 -> String 
                 -> [(String, Attendant)]
                 -> Int 
                 -> MVar FinishRequest
                 -> IO ()
tlsServeWithALPNAndFinishOnRequest certificate_filename key_filename interface_name attendants interface_port finish_request = do 

    let protocols_bs = protocolsToWire $ fmap (\ (s,_) -> pack s) attendants
    withCString certificate_filename $ \ c_certfn -> withCString key_filename $ \ c_keyfn -> withCString interface_name $ \ c_iname -> do 

        connection_ptr <- BU.unsafeUseAsCStringLen protocols_bs $ \ (pchar, len) ->
            makeConnection 
                c_certfn
                c_keyfn
                c_iname
                (fromIntegral interface_port)
                pchar 
                (fromIntegral len)


        let 
            recursion = do 
                either_wired_ptr <- alloca $ \ wired_ptr_ptr -> 
                    let 
                        tryOnce = do 
                            result_code <- waitForConnection connection_ptr smallWaitTime wired_ptr_ptr
                            let 
                                r = case result_code of  
                                    re  | re == allOk        -> do 
                                            p <- peek wired_ptr_ptr
                                            return $ Right_I  p
                                        | re == timeoutReached -> do 
                                            got_finish_request <- tryTakeMVar finish_request
                                            case got_finish_request of 
                                                Nothing ->
                                                    tryOnce
                                                Just _ ->
                                                    return Interrupted 

                                        | re == badHappened  -> return $ Left_I "A wait for connection failed"
                            r 
                    in tryOnce
                    

                case either_wired_ptr of 

                    Left_I msg -> do 
                        putStrLn $ ".. No connection " ++ msg

                        -- // .. //
                        recursion


                    Right_I wired_ptr -> do 
                        let 
                            pushAction datum = BU.unsafeUseAsCStringLen (LB.toStrict datum) $ \ (pchar, len) -> do 
                                result <- sendData wired_ptr pchar (fromIntegral len)
                                case result of  
                                    r | r == allOk           -> return ()
                                      | r == badHappened     -> throwIO $ ConnectionIOError "Could not send data"
                            pullAction = do 
                                allocaBytes useBufferSize $ \ pcharbuffer -> 
                                    alloca $ \ data_recvd_ptr -> do 
                                        result <- recvData wired_ptr pcharbuffer (fromIntegral useBufferSize) data_recvd_ptr
                                        recvd_bytes <- case result of 
                                            r | r == allOk       -> peek data_recvd_ptr
                                              | r == badHappened -> throwIO $ ConnectionIOError "Could not receive data"

                                        B.packCStringLen (pcharbuffer, fromIntegral recvd_bytes)

                        use_protocol <- getSelectedProtocol wired_ptr

                        putStrLn $ ".. Using protocol: " ++ (show use_protocol)

                        let 
                            maybe_session_attendant = case fromIntegral use_protocol of 
                                n | (use_protocol >= 0)  -> Just $ snd $ attendants !! n 
                                  | otherwise          -> Nothing 

                        case maybe_session_attendant of 

                            Just session_attendant -> 
                                session_attendant pushAction pullAction

                            Nothing ->
                                disposeWiredSession wired_ptr

                        -- // .. //
                        recursion 

                    Interrupted -> 
                        return ()

        -- Start the loop defined above...
        recursion 

-- When we are using the eternal version of this function, wake up 
-- each second .... 
defaultWaitTime :: CInt
defaultWaitTime = 1000000

-- Okej, more responsiviness needed 
smallWaitTime :: CInt 
smallWaitTime = 50000