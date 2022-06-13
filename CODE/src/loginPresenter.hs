
module LoginPresenter where


import Graphics.UI.Gtk hiding(get)
import Control.Monad
import Data.IORef
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Glib.Attributes
import LoginConfig
import LoginModel
import LoginView
import ClientPresenter
import PlayPresenter hiding (mainLoop)
import Network.URI
import System.Directory
import PlayConfig (stylePath, ballPath, paddlePath, blockPath)
import EndView

--Lists which files are missing
checkFiles:: IO [String]
checkFiles = do
 let fileList = [stylePath, ballPath, paddlePath, blockPath, tailPicPath, snakeHeadPicPath, loginStylePath]
 noneExitFile   <- sequence $ map lookFileUp fileList
 return (concat noneExitFile)

--check if file exist 
lookFileUp:: String -> IO [String]
lookFileUp file = do
 checkFile <- doesFileExist file
 if  checkFile then do
    return []
 else
    return [file]

--if files are missing the user is notified else start loginmodus
startMain:: IO ()
startMain = do
    initGUI
    checkFiles <- checkFiles
    if length checkFiles > 0 then do
       createEndView ("Missing Files \n" ++ ( show checkFiles))
    else 
      startPresenter



--Starts the loginView
startPresenter:: IO ()
startPresenter = do
  (globalStateRef, window) <- startLoginGui
  handlerId <- timeoutAdd  ( mainLoop globalStateRef) defaultGameSpeed
  handlerIdRef <- newIORef handlerId
  globalState <- readIORef globalStateRef
  listenToUser handlerIdRef (getSlider globalState) globalStateRef globalState window
  widgetShowAll window
  on window deleteEvent $ liftIO mainQuit >> return False
  mainGUI  


--Block to handle User interaction
-- #########################################

--callbacks for interaction
listenToUser:: IORef HandlerId  -> Adjustment ->
               IORef GlobalState  -> GlobalState ->
               Window -> IO (ConnectId Adjustment)
listenToUser handlerIdRef slider globalStateRef globalState window = do
  on (getGameButton globalState)  buttonActivated $ changeWindowToPlay window handlerIdRef globalStateRef
  on (getWatchButton globalState)  buttonActivated $ changeWindowToWatch window handlerIdRef globalStateRef
  on (getUserNameEntry globalState) focusInEvent $ resetUserNameErrorMsg globalStateRef
  on (getPortEntry globalState) focusInEvent $ resetPortErrorMsg globalStateRef
  on (getIpEntry globalState) focusInEvent $ resetIpErrorMsg globalStateRef
  setSliderCallback handlerIdRef slider globalStateRef


--Change speed of ball based on user interaction with slider
setSliderCallback:: IORef HandlerId -> Adjustment -> 
                    IORef GlobalState -> IO (ConnectId Adjustment)
setSliderCallback handlerId slider globalStateRef = onValueChanged slider $ do 
   currentHandlerId <- readIORef handlerId
   value <- adjustmentGetValue slider
   if  (value >= 10)  then do
       timeoutRemove currentHandlerId
       id <-timeoutAdd  (mainLoop globalStateRef) $ round  value 
       writeIORef handlerId id
   else
       adjustmentSetValue slider 10

--Change form loginView to playview
changeWindowToPlay:: Window -> IORef HandlerId -> IORef GlobalState -> IO ()
changeWindowToPlay window id globalStateRef = do
  (check, userName, portNr, ipNr, speed) <- checkUserInput globalStateRef
  when check $ do 
     timerID  <- readIORef id
     timeoutRemove timerID
     (x,y) <- windowGetPosition window
     widgetHide window
     mainQuit
     radioButtonModus <- checkRaidoButton globalStateRef
     startPlayPresenter userName (x,y) portNr ipNr speed radioButtonModus

--Change form loginView to clientView
changeWindowToWatch:: Window -> IORef HandlerId -> IORef GlobalState -> IO ()
changeWindowToWatch window id globalStateRef = do
  (check, userName, portNr, ipNr, _) <- checkUserInput globalStateRef
  when check $ do 
     timerID  <- readIORef id
     timeoutRemove timerID
     (x,y) <- windowGetPosition window
     widgetHide window
     mainQuit
     startClient userName (x,y) portNr ipNr

-- Reset error msg   
-- #############################                               
resetIpErrorMsg:: IORef GlobalState -> EventM EFocus Bool                          
resetIpErrorMsg globalStateRef  = do
  globalState <- liftIO $ readIORef globalStateRef
  ipText <- liftIO $ entryGetText $ getIpEntry globalState
  when (ipText == "IP Parse Error") $ do
     liftIO $ entrySetText (getIpEntry globalState) ""
  return True

resetUserNameErrorMsg:: IORef GlobalState -> EventM EFocus Bool   
resetUserNameErrorMsg globalStateRef = do
  globalState <- liftIO $ readIORef globalStateRef
  userNameText <- liftIO $ entryGetText $ getUserNameEntry globalState
  when (userNameText == "UserName to short!") $ do
     liftIO $ entrySetText (getUserNameEntry globalState) ""
  return True

resetPortErrorMsg:: IORef GlobalState -> EventM EFocus Bool   
resetPortErrorMsg globalStateRef = do
  globalState <- liftIO $ readIORef globalStateRef
  portMsg  <- liftIO $ entryGetText $ getPortEntry globalState
  when (portMsg == "Parse Error " || portMsg == "Port Digits must be 4") $ do
     liftIO $ entrySetText (getPortEntry globalState) ""
  return True            


-- Checks if given username, ip and port is correct
checkUserInput:: IORef GlobalState -> IO (Bool, String, String, String, Int)
checkUserInput globalStateRef = do
  s <- readIORef globalStateRef -- s = currentState 
  value <- adjustmentGetValue (getAdjustment s)
  userName    <-  System.Glib.Attributes.get (getUserNameEntry s) entryText :: IO String
  portNr <- System.Glib.Attributes.get (getPortEntry s) entryText :: IO String
  ipNr <- System.Glib.Attributes.get (getIpEntry s) entryText :: IO String
  portBoolean <-  setportErrMsg (getPortEntry s) portNr
  userNameBoolean <- setUserNameErrMsg (getUserNameEntry s) userName
  ipAddBoolean <- setIPErrMsg (getIpEntry s) ipNr             
  return (portBoolean && userNameBoolean && ( ipAddBoolean), userName, portNr, ipNr, round value)

-- Set error msg
-- #########################################
setIPErrMsg:: Entry -> String -> IO Bool
setIPErrMsg ipEntry ipInput = do
 if not $ isIPv4address ipInput then do
    entrySetText ipEntry "IP Parse Error"
    return False
 else
   return True

setportErrMsg:: Entry -> String -> IO Bool    
setportErrMsg portEntry portInput = do
  case checkValidPort portInput of
       Right _ -> return True
       Left  msgErr -> entrySetText portEntry msgErr >>
                       return False
setUserNameErrMsg:: Entry -> String -> IO Bool                                  
setUserNameErrMsg userNameEntry userInput = do
  case checkUserName userInput of
       Right _ -> return True
       Left msgErr -> entrySetText userNameEntry msgErr >>
                      return False

--get selected modus
checkRaidoButton :: IORef GlobalState -> IO [Char]
checkRaidoButton sRef = do
     s <- readIORef sRef
     normalButtonSet <- toggleButtonGetActive $ getNormalButton s 
     if normalButtonSet then do
        return "Normal"
     else
        return "Reverse"
  

--Block  to handle Snake/Ball movement
-- #############################
mainLoop:: IORef GlobalState -> IO Bool
mainLoop globalStateRef = do
  globalState <- readIORef globalStateRef
  (_, updateState)   <- runStateT  animation globalState
  writeIORef globalStateRef updateState >> return True   
               
animation:: StateT GlobalState IO ()
animation = do
  calHead 
  remove_tail_from_layout 
  addHeadLayout_transformer   

calHead:: StateT GlobalState IO ()
calHead = StateT $ \s -> do
  let currentPositon = snakeMovement (currentLocation s)
  setHead $ head $ picImageList s --	
  return ((), s {currentLocation = currentPositon})

setHead:: Image -> IO ()
setHead head = do
  pix_head <- pixbufNewFromFile tailPicPath 
  imageSetFromPixbuf  head pix_head
          
addHeadLayout_transformer:: StateT GlobalState IO ()          
addHeadLayout_transformer  = StateT $ \s -> do
  let currentList = picImageList s 
  pix_tail <- pixbufNewFromFile snakeHeadPicPath 
  head  <- imageNewFromPixbuf pix_tail         
  let (headX, headY, _) = currentLocation s
  layoutPut (getLayout s) head headX headY
  widgetShow head
  return ((),  s{picImageList = (head:currentList)}) 


remove_tail_from_layout:: StateT GlobalState IO ()
remove_tail_from_layout  = StateT  $ \s -> do 
  let (x:xs) = reverse  $ picImageList s
  let currentXs = reverse xs
  liftIO $ containerRemove (getLayout s) x
  return ((), s {picImageList = currentXs})

snakeMovement:: (Int,Int,String) -> (Int,Int,String)
snakeMovement currentHead@(headX, headY,  dir)
  | headX <  rightBound && dir == "Right" = (headX + deltaMovement, headY, dir) 
  | headX >= rightBound  && dir == "Right" = (headX , headY - deltaMovement, "Up")
  | headY > upBound  && dir == "Up" =   (headX , headY - deltaMovement, dir)
  | headY <= upBound  && dir == "Up" =   (headX - deltaMovement , headY, "Left")
  | headX > leftBound  && dir == "Left" =   (headX - deltaMovement , headY , "Left")
  | headX <= leftBound  && dir == "Left" =   (headX  , headY + deltaMovement , "Down")
  | headY < downBound  && dir == "Down" =   (headX  , headY + deltaMovement , "Down")
  | headY >= downBound  && dir == "Down" =   (headX + deltaMovement , headY  , "Right")
  | otherwise = (headX, headY, dir)
