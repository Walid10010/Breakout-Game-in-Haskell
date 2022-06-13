
{-# LANGUAGE OverloadedStrings #-}

module ClientPresenter where
import Graphics.UI.Gtk
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import PlayModel
import Data.Aeson
import ClientView
import Control.Monad
import Control.Concurrent
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import NetworkModel
import qualified Data.Either as Either
import EndView


--Try to connect with given address. In case of failure user is notified, otherwise we listen to server
startClient:: String -> (Int, Int) -> String -> String -> IO () 
startClient input (x,y) portNr ipNr = withSocketsDo $ do
   initGUI
   addr <- resolve ipNr portNr
   value <- open addr
   case value of 
      Either.Left _ ->  createEndView ("Maybe Wrong IP Address"::String)
      Either.Right sock ->  initClient input (x,y) sock


--In case we established a connection we listen to server msg and update GUI.
startClientPresenter:: String -> (Int, Int) -> Socket -> JsonGlobalState -> IO ()
startClientPresenter input (x,y) sock jsonMsg = do
   (win, globalStatePlay) <- getWindow input jsonMsg
   on win deleteEvent (liftIO mainQuit >> return False)
   on (getSendButton globalStatePlay) buttonPressEvent (sendTextToServer globalStatePlay sock input)
   msgMVar <- newEmptyMVar
   windowMove win x y 
   widgetShowAll  win
   let mapToImage = blockToImage  $ blockList globalStatePlay 
   forkIO $ recvServerMsg msgMVar sock win
   forkIO $ waitForMsg msgMVar globalStatePlay mapToImage
   mainGUI


--starts the clientView GUI based on currentstate of game. 
initClient :: String -> (Int, Int) -> Socket -> IO ()
initClient input (x,y) sock  = do
   msg <- recv sock 5
   let currentLength = read  $ C.unpack msg
   jsonMsg <- readUntil currentLength ""  sock
   let value = decodeStrict jsonMsg ::Maybe JsonGlobalState
   case value of
        Nothing ->  mainQuit >> createEndView ("Connection Error"::String)
        Just decodedMsg -> startClientPresenter input (x,y) sock decodedMsg

-- Resolve a host or service name to one address
resolve:: String -> String -> IO [AddrInfo] 
resolve host port = do
   let hints = defaultHints { addrSocketType = Stream }
   getAddrInfo (Just hints) (Just host) (Just port)
   --return addr

-- Checks if one can connect to given address
open:: [AddrInfo] -> IO (Either String Socket)
open [] = return $ Either.Left "Error Connection"
open (addr:_) = do
   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
   value <- E.try $ connect sock $ addrAddress addr::IO (Either E.SomeException ())
   case value of 
      Either.Right _ -> return $ Either.Right sock
      Either.Left _ -> return $ Either.Left "Error Connection"
        
-- Creates a map for block to Image. We need this to remove block image, if needed  
blockToImage :: [Block] -> Map.Map JsonCoordinate Image      
blockToImage blockList = Map.fromList (map blockToTuple blockList)

blockToTuple :: Block -> (JsonCoordinate, Image)
blockToTuple block = (JsonCoordinate {px=x block, py=y block}, image block)



-- Sends to server user msg
sendTextToServer:: GlobalStatePlay -> Socket -> String -> EventM EButton Bool
sendTextToServer globalStatePlay sock userName = do 
   userText <- liftIO $ entryGetText $ getUserEntry globalStatePlay
   liftIO $ entrySetText ( getUserEntry globalStatePlay) (""::String) >>
            sendAll sock (C.pack (userName ++ ": " ++ userText ++ "\n"))
   return True

-- Waiting for receiving msg and then update GUI
waitForMsg:: MVar JsonGlobalState -> GlobalStatePlay -> Map.Map JsonCoordinate Image -> IO ()
waitForMsg msgMVar globalStatePlay mapToImage = do 
   msg  <-  takeMVar msgMVar
   postGUIAsync $ displayUserMsg (serverMsg  msg) globalStatePlay >>
                   changeBallPos globalStatePlay (ballPos msg) >>
                   changePaddlePos (paddlePos msg) globalStatePlay >>
                   labelSetText (getScoreLabel globalStatePlay) (scoreValue msg)
   let deleteListServer = deleteBlockList msg
   forM deleteListServer (removeFromlayout mapToImage globalStatePlay)
   waitForMsg msgMVar globalStatePlay mapToImage

--Block which update the GUI given server msg
-- ###########################
changePaddlePos:: JsonCoordinate -> GlobalStatePlay -> IO ()
changePaddlePos paddlePos s = do
   paddleX <- takeMVar (paddle s)
   layoutMove (getLayout s) (image paddleX ) (px paddlePos) (py paddlePos)
   putMVar (paddle s) paddleX
 

displayUserMsg:: [String] -> GlobalStatePlay ->  IO ()
displayUserMsg msg s = do
   if  length msg > 0 then do
       sequence $ map (textBufferInsertAtCursor (getTextBuffer s)) msg
       return()
   else 
       return ()   

removeFromlayout:: Map.Map JsonCoordinate Image -> GlobalStatePlay -> 
                   JsonCoordinate -> IO ()
removeFromlayout mapToImage globalStatePlay key =do
   let layoutX = layout $ playViewGui globalStatePlay
   let g = Map.lookup key mapToImage
   case g of
        Just y -> postGUIAsync $ containerRemove layoutX  y
        _  -> return ()

changeBallPos:: GlobalStatePlay -> JsonCoordinate -> IO ()
changeBallPos s ballX = layoutMove (getLayout s) (getImageBall s) (px ballX) (py ballX) 


--Block to collect data from network
-- ################################

-- waiting for new server msg. In case of failure the user is notified 
recvServerMsg:: MVar JsonGlobalState -> Socket -> Window -> IO ()
recvServerMsg msgMVar sock window = do
   msg <- recv sock 5
   if (C.length msg == 0) then do
        postGUIAsync $ widgetHide window >> mainQuit >>
                          createEndView ("Lost Connection"::String)
   else do
      decodeJson msgMVar sock msg window

-- helper function for recvServerMsg. Decodes received msg to JsonGlobalState
decodeJson:: MVar JsonGlobalState -> Socket -> C.ByteString -> Window -> IO ()
decodeJson msgMVar sock msg window = do
   let currentLength = read  $ C.unpack msg
   jsonMsg <- readUntil currentLength ""  sock
   let value = decodeStrict jsonMsg ::Maybe JsonGlobalState
   case value of
        Nothing ->  postGUIAsync $ widgetHide window >> mainQuit >>
                    createEndView ("Connection Error"::String)
        Just y -> 
                 if  read (status y) == Running then
                     putMVar msgMVar y >>
                     recvServerMsg msgMVar sock window
                else
                     postGUIAsync $ widgetHide window >> mainQuit >>
                     createEndView (status y::String)



--helper function to collect data which was sent through tcp
readUntil:: Int -> C.ByteString -> Socket -> IO C.ByteString  
readUntil currentLength total  sock = do
   jsonMsg <- recv sock currentLength
   let diff = currentLength - C.length jsonMsg
   if diff > 0 then 
      readUntil diff (C.append  total jsonMsg) sock
   else
      return (C.append total jsonMsg)     
    