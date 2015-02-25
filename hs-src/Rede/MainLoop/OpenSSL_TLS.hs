{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Rede.MainLoop.OpenSSL_TLS(
    tlsServeWithALPN
    -- ,tlsServeWithALPNOnce
    ) where 



import           Control.Monad
import           Data.Foldable              (foldMap)
import           Data.Monoid                ()
import           Foreign
import           Foreign.C

import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Unsafe     as BU

import           Rede.MainLoop.PushPullType
           

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

-- int wait_for_connection(connection_t* conn, wired_session_t** wired_session);
foreign import ccall "wait_for_connection" waitForConnection :: Connection_Ptr -> Ptr Wired_Ptr -> IO CInt 

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
            wired_ptr <- alloca $ \ wired_ptr_ptr -> do 
                result <- waitForConnection connection_ptr wired_ptr_ptr
                case result of  
                    r | r == allOk        -> peek wired_ptr_ptr
                      | r == badHappened  -> error "Wired failed"
            let 
                pushAction datum = BU.unsafeUseAsCStringLen (LB.toStrict datum) $ \ (pchar, len) -> do 
                    result <- sendData wired_ptr pchar (fromIntegral len)
                    case result of  
                        r | r == allOk           -> return ()
                          | r == badHappened     -> error "Could not send data"
                pullAction = do 
                    allocaBytes useBufferSize $ \ pcharbuffer -> 
                        alloca $ \ data_recvd_ptr -> do 
                            result <- recvData wired_ptr pcharbuffer (fromIntegral useBufferSize) data_recvd_ptr
                            recvd_bytes <- case result of 
                                r | r == allOk       -> peek data_recvd_ptr
                                  | r == badHappened -> error "Could not wait for data"

                            B.packCStringLen (pcharbuffer, fromIntegral recvd_bytes)

            use_protocol <- getSelectedProtocol wired_ptr

            putStrLn $ "Use protocol: " ++ (show use_protocol)

            let 
                maybe_session_attendant = case fromIntegral use_protocol of 
                    n | (use_protocol >= 0)  -> Just $ snd $ attendants !! n 
                      | otherwise          -> Nothing 

            case maybe_session_attendant of 

                Just session_attendant -> 
                    session_attendant pushAction pullAction

                Nothing ->
                    disposeWiredSession wired_ptr

                 
