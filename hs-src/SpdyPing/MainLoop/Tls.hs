-- TLS is small hell to setup, let's do it here ...

module SpdyPing.MainLoop.Tls where 


import Network.Socket 


readyServerSocket :: String -> Int ->  IO Socket 
readyServerSocket hostname portnumber = do 
	the_socket <- socket AF_INET Datagram defaultProtocol 
	addr_info <- getAddrInfo (defaultHints {
		
		})
	bindSocket the_socket (SockAddrInet (PortNum $ fromIntegral portnumber) host_address)

-- 
enchantSocket 