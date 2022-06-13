
module PlayView where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.Style
import Graphics.UI.Gtk.General.StyleProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.Enums
import Graphics.UI.Gtk.Abstract.Widget
import Control.Monad.IO.Class
import PlayModel
--import Config
import PlayConfig
import Control.Concurrent.MVar
import Server


--initialize PlayView
getWindow:: String -> String-> IO (Window, GlobalStatePlay)
getWindow userName modus = do
   window   <- windowNew
   hPaned   <- hPanedNew
   layout   <- layoutNew Nothing Nothing
   blockList <- sequence  $ setBlockOnScreen layout
   ball <- setBallonScreen layout
   paddle <-setPaddleonScreen layout
   (textBuffer, textView) <-  setTextView
   setCSSonScreen
   (score, userNameLabel, sendButton, userEntry) <- setPosHbox textView userName hPaned layout
   widgetShow textView
   panedSetPosition hPaned splitHpane
   setWindowsAtt window hPaned
   widgetShowAll window
   let playViewGui = PlayViewGui{score=score, textBuffer=textBuffer, 
                                textView=textView, layout=layout, 
                                userNameLabel=userNameLabel,                        
                                sendButton=sendButton, userEntry=userEntry}
   let movement = Movement{vX= PlusDir delta, vY= NegDir delta}
   dir <- newEmptyMVar
   putMVar dir paddle
   clientInfoListMVar <- newEmptyMVar
   putMVar clientInfoListMVar ([]::[ClientInfo])
   msgListMVar <- newEmptyMVar 
   putMVar msgListMVar []
   on hPaned buttonPressEvent (liftIO $ panedSetPosition hPaned splitHpane >> return True)  
   return (window, GlobalStatePlay{blockList=blockList, deleteList=[],
                                   paddle=dir, ball=ball, 
                                   playViewGui=playViewGui, 
                                   movement=movement,  
                                   clientList=clientInfoListMVar, 
                                   msgList=msgListMVar, modus=read modus, 
                                   gameStatus=Running})


setBallonScreen:: Layout -> IO Ball 
setBallonScreen layout = do
   pix_tail <- (pixbufNewFromFile ballPath) 
   image <- imageNewFromPixbuf pix_tail
   layoutPut layout image ballXStart ballYStart
   return  Ball {imageBall=image, radius=ballRadius, xBall=ballXStart, yBall=ballYStart}

setPaddleonScreen:: Layout -> IO Block
setPaddleonScreen layout = do
   pix_tail <- pixbufNewFromFileAtScale  paddlePath paddleWidth paddleHeight False
   image <- imageNewFromPixbuf pix_tail
   layoutPut layout image paddleStartX paddleStartY
   return  Block {image=image, x=paddleStartX, y=paddleStartY}

setBlockOnScreen::Layout -> [IO Block]
setBlockOnScreen layout =
   [setBlockImage currentX currentY layout | currentX <- [0..9], currentY <- [0..9]]

setBlockImage:: Int -> Int -> Layout -> IO Block
setBlockImage currentX currentY layout = do
   image <-createImage blockPath 
   layoutPut layout image (currentX * blockWidth ) (currentY * blockHeight )
   return Block {image=image, x=currentX * blockWidth, y=currentY * blockHeight}

createImage:: String -> IO Image
createImage src = do
   pix_tail <- pixbufNewFromFileAtScale src blockWidth blockHeight False
   image <- imageNewFromPixbuf pix_tail
   return image

setPosHbox:: TextView -> String -> HPaned -> Layout -> 
             IO (Label, Label, Button, Entry)     
setPosHbox textView userName hPaned layout = do
   hbox     <- vBoxNew False 0
   hboxSend <- hBoxNew False 0
   vboxGame <- vBoxNew False 0
   score <- labelNew $ Just "Score 0"
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


setTextView:: IO(TextBuffer, TextView) 
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
   set window [windowResizable :=False,
               windowTitle := "Game",
               containerChild := hPaned,
               windowWindowPosition := WinPosCenterAlways]

--load css style 
setCSSonScreen::IO ()
setCSSonScreen = do 
   (Just screen) <- screenGetDefault
   css <- cssProviderNew
   cssProviderLoadFromPath css stylePath
   styleContextAddProviderForScreen  screen css 800