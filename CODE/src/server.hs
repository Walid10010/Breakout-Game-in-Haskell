module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, void)
import qualified Data.ByteString as S
import Network.Socket hiding(recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent.MVar
import Data.ByteString.Char8 hiding(putStrLn)
import qualified Data.Either as Either


data ClientInfo = ClientInfo {socketClient::Socket, sockAddr::SockAddr} deriving(Show)

-- Resolve a host or service name to one or more addresses
resolve:: String -> String -> IO [AddrInfo]
resolve host port = do
   let hints = defaultHints { addrSocketType = Stream }
   getAddrInfo (Just hints) (Just host) (Just port)


-- Try to open server on given address
open:: [AddrInfo] -> IO (Either String Socket)
open []  = return $ Either.Left "Connection Error"     
open (addr:_) = do
   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
   setSocketOption sock ReuseAddr 1
   value <-E.try $ bind sock (addrAddress addr):: IO(Either E.SomeException ())
   case value of
         Either.Right _ -> return $ Either.Right sock
         Either.Left _ -> return $ Either.Left "Sock Bind Error"

-- Listen to incoming connection from client 
loop:: MVar [ClientInfo] -> MVar [String] -> Socket -> IO ()
loop clientInfoListMVar msgListMVar sock = do
   value <- E.try $ accept sock ::IO(Either E.SomeException (Socket, SockAddr))
   case value of 
        Left _ -> return ()
        Right (conn, peer) -> do
                       clientInfoList  <- takeMVar clientInfoListMVar
                       let currentClient = ClientInfo {socketClient=conn, sockAddr= peer}
                       putMVar clientInfoListMVar (currentClient:clientInfoList)
                       putStrLn $ "Connection from " ++ show peer
                       void $ forkFinally (listenToClient msgListMVar conn) (\_ -> close conn)
                       loop clientInfoListMVar msgListMVar sock

-- Waiting to receive msg from client
listenToClient:: MVar [String] -> Socket -> IO ()
listenToClient msgListMVar conn = do
   msg <- recv conn 1024
   msgList <- takeMVar msgListMVar
   putMVar msgListMVar ((unpack msg):msgList)
   unless (S.null msg) $ do
        listenToClient msgListMVar conn