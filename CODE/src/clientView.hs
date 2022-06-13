
module ClientView where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.Style
import Graphics.UI.Gtk.General.StyleProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.Enums
import Control.Monad.IO.Class
import PlayModel
--import Config
import PlayConfig
import Control.Concurrent.MVar
import Server
import NetworkModel

--initialize ClientView
getWindow :: String-> JsonGlobalState -> IO (Window, GlobalStatePlay) 
getWindow userName globalJsonMsg = do
  window   <- windowNew
  hPaned   <- hPanedNew
  layout   <- layoutNew Nothing Nothing
  let currentBlock = blockListServer globalJsonMsg
  let ballPosX = ballPos globalJsonMsg
  let paddlePosX = paddlePos globalJsonMsg 
  blockList <- sequence  $ setBlockOnScreen layout currentBlock
  ball <- setBallonScreen layout ballPosX
  paddle <-setPaddleonScreen layout paddlePosX
  (textBuffer, textView) <-  setTextView
  setCSSonScreen
  (score, userNameLabel, sendButton, userEntry) <- setPosHbox textView userName hPaned layout
  widgetShow textView
  panedSetPosition hPaned splitHpane
  setWindowsAtt window hPaned
  widgetShowAll window
  let playViewGui = PlayViewGui{score=score, textBuffer=textBuffer,
                                textView=textView,layout=layout, 
                                userNameLabel=userNameLabel, 
                                sendButton=sendButton, 
                                userEntry=userEntry}  
  let movement = Movement{vX= PlusDir 1, vY= NegDir 1}
  dir <- newEmptyMVar
  putMVar dir paddle
  clientInfoListMVar <- newEmptyMVar
  putMVar clientInfoListMVar ([]::[ClientInfo])
  msgListMVar <- newEmptyMVar 
  putMVar msgListMVar []
  on hPaned buttonPressEvent $ liftIO $ panedSetPosition hPaned splitHpane >>
                               return True  
  return (window, GlobalStatePlay{blockList=blockList, deleteList=[], 
                                  paddle=dir, ball=ball, 
                                  playViewGui=playViewGui, 
                                  movement=movement, 
                                  clientList=clientInfoListMVar, 
                                  msgList=msgListMVar, 
                                  modus=ClientModus,
                                  gameStatus=Running})

setBallonScreen:: Layout -> JsonCoordinate -> IO Ball
setBallonScreen layout ballPos = do
     pix_tail <- pixbufNewFromFile "res/ball.png" 
     image <- imageNewFromPixbuf pix_tail
     layoutPut layout image ballXStart ballYStart
     return  Ball {imageBall=image, radius=ballRadius, xBall=px ballPos, yBall=py ballPos}

setPaddleonScreen :: Layout -> JsonCoordinate -> IO Block
setPaddleonScreen layout paddlePos = do
     pix_tail <- pixbufNewFromFile "res/paddle.png"
     image <- imageNewFromPixbuf pix_tail
     layoutPut layout image paddleStartX paddleStartY
     return  Block {image=image, x=px paddlePos, y= paddleStartY}

setBlockOnScreen:: Layout  -> [JsonCoordinate] -> [IO Block]
setBlockOnScreen layout blockList =
    [ setBlockImage block layout | block <- blockList]

setBlockImage:: JsonCoordinate -> Layout -> IO Block
setBlockImage block layout = do
     image <-createImage "res/block.png"
     layoutPut layout image ( px block) (py block)
     return Block {image=image, x=px block, y=py block}

createImage:: String -> IO Image
createImage src = do
     pix_tail <- pixbufNewFromFileAtSize src blockWidth blockHeight
     image <- imageNewFromPixbuf pix_tail
     return image


setPosHbox:: TextView -> String -> HPaned -> Layout ->
             IO (Label, Label, Button, Entry)     
setPosHbox textView userName hPaned layout = do
  hbox     <- vBoxNew False 0
  hboxSend <- hBoxNew False 0
  vboxGame <- vBoxNew False 0
  score <- labelNew $ Just "Score: 11250"
  userLabel <- labelNew $ Just userName
  widgetSetSizeRequest score 20 20
  sendButton     <- buttonNewWithLabel "send"
  widgetSetSizeRequest sendButton 20 20
  userInput  <- entryNew
  entrySetWidthChars userInput 10
  boxPackStart hbox userLabel PackNatural 0
  boxPackStart hbox score PackNatural 15
  boxPackStart hbox textView PackGrow 0
  boxPackStart hboxSend userInput PackGrow 0
  boxPackStart hboxSend sendButton PackNatural 0
  boxPackStart hbox hboxSend PackNatural 0
  boxPackStart vboxGame layout PackGrow 0
  panedAdd1 hPaned vboxGame
  panedAdd2 hPaned hbox
  return (score, userLabel, sendButton, userInput)

setTextView:: IO (TextBuffer, TextView)
setTextView = do
  textBuffer <- textBufferNew Nothing
  textView <- textViewNew
  textViewSetIndent textView 4
  textViewSetBuffer textView textBuffer 
  textViewSetCursorVisible textView False
  textViewSetWrapMode textView WrapChar
  textViewSetEditable textView False
  return (textBuffer, textView)

setWindowsAtt:: Window -> HPaned -> IO ()
setWindowsAtt window hPaned = do
 windowSetDefaultSize window  windowsWidth windowsHeight
 set window [ windowResizable :=False,
              windowTitle := "ClientModus",
              containerChild := hPaned,
              windowWindowPosition := WinPosCenterAlways]

setCSSonScreen:: IO ()
setCSSonScreen = do 
  (Just screen)  <- screenGetDefault
  css <- cssProviderNew
  cssProviderLoadFromPath css "res/payView.css"
  styleContextAddProviderForScreen  screen css 800