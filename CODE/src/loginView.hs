
module LoginView where

import Graphics.UI.Gtk hiding(get)
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.Style
import Graphics.UI.Gtk.General.StyleProvider
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.Enums
import Control.Monad
import Data.IORef
import System.Glib.Attributes
import LoginConfig
import LoginModel
import Foreign.C.Types

--initialize LoginView
startLoginGui :: IO (IORef GlobalState, Window)
startLoginGui = do
   initGUI
   window  <- windowNew
   layout  <- layoutNew Nothing Nothing
   setTitleLabelOnScreen layout
   loginGui <- setGridOnScreen layout 
   set window [windowTitle := "LoginView",
               containerChild := layout]
   setCssOnScreen
   windowSetPosition window WinPosCenterAlways
   windowSetDefaultSize window windowsHeight windowsWidth
   globalState <- animatedSnake layout loginGui
   globalStateRef <- newIORef globalState
   return (globalStateRef,  window)


setTitleLabelOnScreen:: Layout -> IO()
setTitleLabelOnScreen layout= do
   titleLabel <- labelNew $ Just "Breakout"
   layoutPut layout titleLabel 175 75


setCssOnScreen:: IO()
setCssOnScreen = do
   Just screen  <- screenGetDefault
   css <- cssProviderNew
   cssProviderLoadFromPath css loginStylePath
   styleContextAddProviderForScreen  screen css defaultCssPriority


setGridOnScreen:: Layout -> IO LoginGui
setGridOnScreen layout = do
   grid     <- gridNew
   userNameLabel <- labelNew $ Just "Username:"
   label2 <- labelNew $ Just "Port:"
   layoutPut layout grid 100  200
   gameButton  <- buttonNewWithLabel "Game Modus"
   watchButton  <- buttonNewWithLabel "Watch Modus" 
   userNameError  <- labelNew $ Just "                  "
   portError  <- labelNew $ Just "                  "
   userNameInput <- entryNew
   portInput   <- entryNew 
   ipLabel <- labelNew $ Just "IP:"
   ipEntry <- entryNew
   gridSetRowBaselinePosition grid 0 BaselinePositionCenter
   gridAttach grid userNameLabel 0 0 1 1
   gridAttach grid userNameInput 1 0 1 1
   gridAttach grid portInput 1 1 1 1
   gridAttach grid label2  0 1 1 1
   gridAttach grid ipLabel 0 2 1 1
   gridAttach grid ipEntry 1 2 1 1
   gridAttach grid gameButton 0 6 1 1
   gridAttach grid watchButton 1 6 1 1
   radio1 <- radioButtonNewWithLabel "   Normal"
   gridAttach grid radio1 0 3 1 1
   radio2 <- radioButtonNewWithLabelFromWidget radio1  "   Reverse"
   gridAttach grid radio2 1 3 1 1
   gridSetColumnSpacing grid 25
   gridSetRowSpacing grid 25
   adj <- adjustmentNew 60 0 100 10 10 10
   scroll <- hScaleNew adj
   gridAttach grid scroll 0 4 2 2
   toggleButtonSetActive radio1 True
   return LoginGui {adjustment=adj, nameEntry=userNameInput, portEntry=portInput, 
                   gameButton=gameButton, watchButton=watchButton,
                   userNameError=userNameError, portError=portError, 
                   ipEntry=ipEntry, sliderValue=defaultGameSpeed,
                   normalButton=radio1, reverseButton=radio2,
                   layout=layout}

                                  
createImageList:: String -> Int -> IO Image
createImageList src = \_ -> do
   pix_tail <- pixbufNewFromFile src
   imageNewFromPixbuf pix_tail

addSnakeToLayout:: [Image] -> (Int, Int) -> Layout -> IO [()]
addSnakeToLayout snake (startX,startY) layout = do
   forM [0..3] (\index -> 
        layoutPut layout (snake !! index) (startX -  12*index - deltaMovement) startY ) 

animatedSnake:: Layout -> LoginGui -> IO GlobalState
animatedSnake layout loginGui = do
   let startpoint = (defaultStartPosX, defaultStartPosY)
   tailList   <- forM [1..3] $ createImageList tailPicPath
   head       <- (createImageList snakeHeadPicPath) 1
   let snake = head : tailList
   addSnakeToLayout snake  startpoint layout
   let headPos  = (defaultStartPosX, defaultStartPosY, "Right")
   let snakeRef  =  snake
   return GlobalState {picImageList=snakeRef, currentLocation=headPos,
                           interactiveGui=loginGui, handlerId=(CUInt 1)}






