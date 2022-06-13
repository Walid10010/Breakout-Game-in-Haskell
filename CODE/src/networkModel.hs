{-# LANGUAGE OverloadedStrings #-}


module NetworkModel where

import PlayModel
import Data.Aeson
import Control.Concurrent.MVar
import qualified  Data.Either as Either
import Control.Exception
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 hiding(putStrLn, length, concat, drop, map)
import Control.Monad
import Network.Socket.ByteString (recv, sendAll)
import Graphics.UI.Gtk
import  Server


--Define data structure to send over network
-- #########################################  

data JsonCoordinate = JsonCoordinate {px::Int, py::Int} deriving (Eq, Ord, Show)

instance FromJSON JsonCoordinate where
   parseJSON = withObject "jsonCoordinate" $ \o ->
      JsonCoordinate  <$> o .: "px"
                      <*> o .: "py"

instance ToJSON JsonCoordinate where
   toJSON p = object [
     "px" .= px p,
     "py"  .= py p]


data JsonGlobalState = JsonGlobalState {
                           blockListServer::[JsonCoordinate],
                           deleteBlockList::[JsonCoordinate],
                           paddlePos::JsonCoordinate,
                           ballPos:: JsonCoordinate,
                           scoreValue::String,
                           serverMsg::[String],
                           status::String} deriving (Show)

instance FromJSON JsonGlobalState where
   parseJSON = withObject "jsonGlobalState" $ \o ->
      JsonGlobalState <$> o .: "blockListServer"
                      <*> o .: "deleteBlockList"
                      <*> o .: "paddlePos"
                      <*> o .: "ballPos"
                      <*> o .: "scoreValue"
                      <*> o .: "serverMsg"
                      <*> o .: "status"



instance ToJSON JsonGlobalState where
   toJSON p = object [
     "blockListServer" .= blockListServer p,
     "deleteBlockList" .= deleteBlockList p,
     "paddlePos"  .= paddlePos  p, 
     "ballPos" .= ballPos p,
     "scoreValue" .= scoreValue p,
     "serverMsg" .= serverMsg p,
     "status" .= status p ]
 

--Send Data to Client
-- ######################################


--Converts GlobalStatePlay data to JsonGlobalState
convertGlobalStatePlaytoJson:: GlobalStatePlay -> IO JsonGlobalState
convertGlobalStatePlaytoJson globalStatePlay = do
   let jsonCoordinateList = map blockTOJson $ blockList globalStatePlay
   let deleteListX = map blockTOJson $ deleteList globalStatePlay
   let paddleMVar = paddle globalStatePlay
   paddle <- takeMVar paddleMVar
   putMVar paddleMVar paddle
   score <- labelGetText (getScoreLabel globalStatePlay)
   let paddlePos = JsonCoordinate { px=x $ paddle, py=y $ paddle} 
   let ballPos = JsonCoordinate { px=xBall $ ball globalStatePlay,
                                  py=yBall $ ball globalStatePlay} 
   currentMsgList <- takeMVar (msgList globalStatePlay)
   putMVar (msgList globalStatePlay) []
   return JsonGlobalState{blockListServer=jsonCoordinateList, 
                          deleteBlockList=deleteListX, 
                          paddlePos=paddlePos, ballPos=ballPos, 
                          scoreValue=score, serverMsg=currentMsgList,
                          status=show $ gameStatus globalStatePlay}

blockTOJson :: Block -> JsonCoordinate
blockTOJson block = JsonCoordinate{px=x block, py=y block}


--Send current game state to all connected clients
sendDataToClient:: GlobalStatePlay -> IO ()
sendDataToClient globalStatePlay = do
   let clientListMVar = clientList globalStatePlay
   clientList <- takeMVar clientListMVar
   jsonGlobalState <- convertGlobalStatePlaytoJson globalStatePlay
   printuserMsg (serverMsg jsonGlobalState) globalStatePlay 
   connectedClient <- forM clientList (sendToClient  jsonGlobalState)
   putMVar clientListMVar $ concat connectedClient 


--helper function to send data to one client
sendToClient:: JsonGlobalState -> ClientInfo  -> IO [ClientInfo]        
sendToClient jsonGlobalState clientInfo = do
   currentLength <-  fitLength jsonGlobalState
   tryToSend <- try $ sendAll (socketClient clientInfo) $  C.pack currentLength 
                           :: IO (Either SomeException ())
   case tryToSend of
        Either.Left _ -> return [] 
        Either.Right _ -> do
                          value  <- try (sendAll (socketClient clientInfo) 
                                    $ toStrict $ encode jsonGlobalState) 
                                    :: IO (Either SomeException ())
                          case value of
                                 Either.Right _ -> return [clientInfo]
                                 Either.Left  _ -> return []

--Calculate how many bytes we have to send
fitLength:: JsonGlobalState -> IO String
fitLength jsonGlobalState = do
   let currentLength = C.length $ toStrict $ encode jsonGlobalState
   let countdigit = length $ show currentLength
   return ((drop countdigit "00000") ++ show currentLength )

--Display user msg
printuserMsg:: [String] -> GlobalStatePlay -> IO [()]
printuserMsg msg s = do
   if length msg > 0 then do
       sequence $ map (textBufferInsertAtCursor (getTextBuffer s)) msg
   else
     return []
