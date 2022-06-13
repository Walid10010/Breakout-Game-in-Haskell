{-# LANGUAGE OverloadedStrings #-}

module PlayPresenter where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Graphics.UI.Gtk
import PlayView
import Data.IORef
import PlayModel
import PlayConfig
import Prelude hiding(Left, Right)
import System.Glib
import Control.Concurrent
import Server
import Control.Monad
import qualified  Data.Either as Either
import NetworkModel
import EndView
import Network.Socket hiding(recv)



--Try to start server on given address. In case of failure the user is notified 
startPlayPresenter:: String -> (Int, Int) -> String -> String -> Int -> String -> IO ()
startPlayPresenter input (x,y) portNr ipNr speed modus = do
   addr <- resolve ipNr portNr
   value     <- open addr :: IO (Either String Socket)
   case value of
      Either.Right sock -> startPlay input (x,y) sock speed modus
      Either.Left _ -> createEndView ("Maybe Wrong IP Address"::String)

--In case of success we start Breakout game and server
startPlay:: String -> (Int, Int) -> Socket -> Int -> String -> IO ()
startPlay input (x,y) sock speed modus = do
   listen sock 10
   (win, globalStatePlay) <- getWindow input modus
   let clientInfoListC =  clientList globalStatePlay
   let msgListC = msgList globalStatePlay
   forkIO $ loop clientInfoListC msgListC sock
   windowMove win x y 
   widgetShowAll  win 
   globalStateRef <- newIORef globalStatePlay
   timeoutAdd  (mainLoop globalStateRef win sock) speed
   let paddleMVar = paddle globalStatePlay
   let layoutX = layout $ playViewGui globalStatePlay
   on win deleteEvent $ liftIO  mainQuit >> return False
   on win keyPressEvent $ catchUserKeyEvent paddleMVar layoutX
   widgetAddEvents win [PointerMotionMask]
   mainGUI


--MainLoop of Breakout-Game         
mainLoop::IORef GlobalStatePlay -> Window -> Socket -> IO Bool
mainLoop globalStatePlayRef window sock = do
   globalStatePlay <- readIORef globalStatePlayRef
   (_, updateState) <- runStateT game globalStatePlay
   if getBallYPos globalStatePlay  >= windowsHeight then 
       sendDataToClient (updateState{gameStatus=GameOver})  >> close sock >>
       widgetHide window >> mainQuit >> 
                  createEndView ("Game Over"::String) >> return False
   else
       if length (blockList globalStatePlay) == 0 then 
          sendDataToClient (updateState{gameStatus=Win}) >> close sock >>
          widgetHide window >> mainQuit 
           >> createEndView ("Win"::String) >> return False
       else
          sendDataToClient updateState >>
          writeIORef globalStatePlayRef updateState >>
          return True   

--Runs one iteration of loop
game::StateT GlobalStatePlay IO ()
game = do
   updateBallPos
   collisiondetectionWall
   collisiondetectionPaddle 
   collisiondetectionBlock
   setLabelScore



--Block to handle update ball position          
-- ##################################################################

-- update ball position  based on current movement      
updateBallPos::StateT GlobalStatePlay IO ()          
updateBallPos = StateT $ \s -> do
   let (newBallX, newBallY) = calcNewBallPos (getMovementTuple s) (getBallPos s)
   layoutMove (getLayout s) (getImageBall s) newBallX newBallY
   let newBall = (ball s) {xBall = newBallX, yBall= newBallY}
   return ((), s {ball=newBall})                 

calcNewBallPos :: (MovementDirection, MovementDirection)  -> 
                  (Int, Int) -> (Int, Int)
calcNewBallPos (PlusDir x , PlusDir _)  (ballX, ballY) = (ballX + x, ballY - delta)
calcNewBallPos (PlusDir x, NegDir _)  (ballX, ballY) = (ballX + x, ballY + delta)
calcNewBallPos (NegDir x, PlusDir _)  (ballX, ballY) = (ballX - x, ballY - delta)
calcNewBallPos (NegDir x, NegDir _)  (ballX, ballY) = (ballX - x, ballY + delta)


--Block to handle interaction between wall and ball          
-- ##################################################################
collisiondetectionWall:: StateT GlobalStatePlay IO ()
collisiondetectionWall = StateT $ \s -> do
   case checkWall (getBallPos s) of 
        NoneHit -> return ((), s)
        value  -> do
                   (_, updateState) <- changeMovementAfterCollision value s False
                   if value == Hit Down  then 
                      setBallAfterWallCollision value "" updateState
                   else 
                       if modus s == Reverse then
                          setBallAfterWallCollision value "reverse" s
                       else
                          setBallAfterWallCollision value "normal" updateState
  

-- Checks collision between ball and wall
checkWall:: (Int, Int) -> Hit
checkWall (ballX, ballY)
   | ballX <= 0 =   Hit Right
   | diffWindowsWidth  <= ballX + 2 * ballRadius = Hit Left
   | ballY  <= 0 =  Hit Down
   | otherwise = NoneHit   


-- Correct ball position after wall hit
setBallAfterWallCollision:: Hit -> String -> GlobalStatePlay -> IO ((), GlobalStatePlay)
setBallAfterWallCollision (Hit Down) _ s = do
   let newBall = (ball s){yBall=0}
   layoutMove (getLayout s) (getImageBall s ) (getBallXPos s) 0
   return ((),s{ball=newBall})

setBallAfterWallCollision (Hit Right) "reverse" s = do
   let diffX = -(2*ballRadius - diffWindowsWidth)
   let newBall = (ball s){xBall= diffX}
   layoutMove (getLayout s) (getImageBall s ) diffX (getBallYPos s)
   return ((),s{ball=newBall})

setBallAfterWallCollision (Hit Left) "reverse" s = do
   let newBall = (ball s){xBall=0}
   layoutMove (getLayout s) (getImageBall s ) 0 (getBallYPos s)
   return ((),s{ball=newBall})

setBallAfterWallCollision (Hit Left) "normal" s = do
   let diffX = -(2*ballRadius - diffWindowsWidth)
   let newBall = (ball s){xBall= diffX}
   layoutMove (getLayout s) (getImageBall s ) diffX (getBallYPos s)
   return ((),s{ball=newBall})

setBallAfterWallCollision (Hit Right) "normal" s = do
   let newBall = (ball s){xBall=0}
   layoutMove (getLayout s) (getImageBall s ) 0 (getBallYPos s)
   return ((),s{ball=newBall})

setBallAfterWallCollision _  _ s = return ((), s)

-- Set current score
setLabelScore::StateT GlobalStatePlay IO ()
setLabelScore = StateT $ \s -> do
   labelValue <-  labelGetText  (getScoreLabel s)
   let currentScore = read $ drop 6 labelValue
   let upDateValue  = show $ currentScore + ( length $ deleteList s)
   labelSetText (getScoreLabel s) ("Score " ++ upDateValue )
   return ((), s)



--Block to handle interaction between block and ball          
-- ##################################################################

collisiondetectionBlock:: StateT GlobalStatePlay IO ()
collisiondetectionBlock = StateT $ \s -> do
   let (hitBlockList, noneHitBlock, hitList) = checkBlockList (blockList s) ([],[],[]) (ball s) (movement s)
   forM hitBlockList (removeBlockFromLayout (getLayout s))
   let updateState = s {blockList = noneHitBlock, deleteList= hitBlockList}
   if (length hitList > 0) then do
       let index = maxIndex hitList (NoneHit, 0, 0)
       (_, updateState) <- changeMovementAfterCollision  (hitList !! index)  updateState (length hitList == 3)
       setBallBlockAfterCollision (hitList !! index) (hitBlockList !! index) updateState                 
   else
      return ((), updateState)


--checkBlockList calculates  with which blocks there was a collision
checkBlockList:: [Block] ->([Block], [Block], [Hit]) -> Ball -> Movement -> ([Block], [Block], [Hit])
checkBlockList [] (a,b,c) _  _  = (a,b,c)
checkBlockList (x:xs) (a,b,c) ball movement
   | currentHitValue == NoneHit = checkBlockList xs (a, x:b, c) ball movement
   | otherwise = checkBlockList xs (x:a, b, currentHitValue:c) ball movement
   where
        currentHitValue = checkBlock ball x movement                                                       

-- Checks collision between ball and block
checkBlock:: Ball -> Block -> Movement -> Hit
checkBlock ball block movement  
   | checkCollision &&  checkDownHit && vY movement == PlusDir 1 = Hit Down
   | checkCollision &&  checkUpHit && vY movement == NegDir 1 = Hit Up
   | checkCollision &&  vX movement == PlusDir 1 = Hit Left
--   | checkCollision && checkUpHit && vY movement == NegDir 1 = Hit Up
   | checkCollision = Hit Right 
   | otherwise = NoneHit
   where
        checkCollision = rectangleIntersection (createBlockRectangle block) (createBallRectangle ball)
        checkDownHit = yBall ball <=  getBlockYHeight block && yBall ball >=  getBlockYHeight block - delta 
        checkUpHit = ballYRadius >=  y block && ballYRadius <=  y block  + delta
        ballYRadius = yBall ball + 2 * ballRadius
 
--Correct ball position after collision with block        
setBallBlockAfterCollision:: Hit -> Block -> GlobalStatePlay -> IO ((), GlobalStatePlay)
setBallBlockAfterCollision  (Hit Down) block s = do
   let ballNew = (ball s) {yBall= getBlockYHeight block}
   layoutMove (getLayout s) (getImageBall s) (getBallXPos s) (getBlockYHeight block)
   return ((), s{ball=ballNew})

setBallBlockAfterCollision  (Hit Up) block s = do
   let diffY = -(2*ballRadius - y block)  
   let ballNew = (ball s) {yBall=diffY}
   layoutMove (getLayout s) (getImageBall s) (getBallXPos s) (diffY)
   return ((), s{ball=ballNew})

setBallBlockAfterCollision  (Hit Left) block s = do
   let diffX = -(2*ballRadius - x block)  
   let ballNew = (ball s) {xBall=diffX}
   layoutMove (getLayout s) (getImageBall s) (diffX) (getBallYPos s)
   return ((), s{ball=ballNew})

setBallBlockAfterCollision  (Hit Right) block s = do
   let ballNew = (ball s) {xBall= getBlockXWidth block}
   layoutMove (getLayout s) (getImageBall s) (getBlockXWidth block) (getBallYPos s)
   return ((), s{ball=ballNew})

setBallBlockAfterCollision  NoneHit _  s =  return ((), s)                                               

--After collision remove block from Layout
removeBlockFromLayout:: Layout -> Block -> IO ()
removeBlockFromLayout layout block =  containerRemove layout  $ image block

--Block to handle interaction between paddle and ball          
-- ##################################################################
collisiondetectionPaddle :: StateT GlobalStatePlay IO ()
collisiondetectionPaddle = StateT $ \s -> do
   currentPaddle <- takeMVar (paddle s)
   let currentMovement = movement s
   let paddleCollision = checkPaddle (ball s) currentPaddle currentMovement
   putMVar (paddle s) currentPaddle
   case paddleCollision of
        NoneHit -> return ((), s)
        value -> do 
                (_, stateNew) <- changeBallVelocityAfterPaddleCollision  value currentPaddle s (getMovementTuple s)
                setBallafterPaddleCollision value stateNew
                                           

-- Checks if theres a collision between paddle and ball
checkPaddle:: Ball -> Block -> Movement -> Hit
checkPaddle ball paddle movement
    | checkCollision && ballYHeight >= y paddle && ballYHeight <= delta + y paddle  = Hit Up
    | checkCollision && (vX movement) == NegDir 1 = Hit Right
    | checkCollision = Hit Left
    | otherwise = NoneHit
     where
        checkCollision =  rectangleIntersection (createBallRectangle ball) (createPaddleRectangle paddle)
        ballYHeight = ballRadius * 2 + yBall ball
        currentDelta = unPackMovement (vX movement)

-- Correct ball position after collision with paddle
setBallafterPaddleCollision:: Hit -> GlobalStatePlay -> IO ((), GlobalStatePlay)
setBallafterPaddleCollision (Hit Up) s  =  do
   let diffY = (2*ballRadius + getBallYPos s) -  paddleStartY
   let ballNew = (ball s) {yBall= getBallYPos s - diffY}
   layoutMove (getLayout s) (getImageBall s) (getBallXPos s) (getBallYPos s - diffY)
   return ((), s{ball=ballNew})

setBallafterPaddleCollision (Hit Left) s =  do
   let paddleMVar = paddle s
   currentPaddle <- takeMVar paddleMVar
   let ballNew = (ball s) {xBall=x currentPaddle - 2*ballRadius}
   layoutMove (getLayout s) (getImageBall s) (x currentPaddle - 2*ballRadius) (getBallYPos s)
   putMVar paddleMVar currentPaddle
   return ((), s{ball=ballNew})

setBallafterPaddleCollision (Hit Right) s = do
   let paddleMVar = paddle s
   paddle <- takeMVar paddleMVar
   let ballNew = (ball s) {xBall= x paddle + paddleWidth }
   layoutMove (getLayout s) (getImageBall s) (x paddle + paddleWidth) (getBallYPos s)
   putMVar paddleMVar paddle
   return ((), s{ball=ballNew})
                                      
setBallafterPaddleCollision _ s  = return((), s)


--Calculate new velocity for ball
changeBallVelocityAfterPaddleCollision:: Hit -> Block -> GlobalStatePlay -> 
                                         (MovementDirection, MovementDirection) -> 
                                         IO ((), GlobalStatePlay)
changeBallVelocityAfterPaddleCollision   (Hit Up) paddle s (PlusDir delX, PlusDir delY) = do
   let newVelocity = calVelocity paddle (getBallXPos s) delX
   let newMovement = (movement s){vX= PlusDir newVelocity, vY= NegDir delY}
   return ((), s{movement=newMovement})

changeBallVelocityAfterPaddleCollision   (Hit Up) paddle s (PlusDir delX, NegDir delY) = do
   let newVelocity = calVelocity paddle (getBallXPos s) delX
   let newMovement = (movement s ){vX=PlusDir newVelocity, vY= PlusDir delY}
   return ((), s{movement=newMovement})

changeBallVelocityAfterPaddleCollision   (Hit Up) paddle s (NegDir delX, PlusDir delY) = do
   let newVelocity = calVelocity paddle (getBallXPos s) delX
   let newMovement = (movement s){vX=NegDir newVelocity, vY= NegDir delY}
   return ((), s{movement=newMovement})

changeBallVelocityAfterPaddleCollision   (Hit Up) paddle s (NegDir delX, NegDir delY) = do
   let newVelocity = calVelocity paddle (getBallXPos s) delX
   let newMovement = (movement s){vX=NegDir newVelocity, vY=PlusDir delY}
   return ((), s{movement=newMovement})

changeBallVelocityAfterPaddleCollision  value _ s _ = changeMovementAfterCollision value  s False

calVelocity :: Block -> Int -> Int -> Int         
calVelocity currentPaddle ballX currentMovementX
   | middlePaddlePointX < ballX && addVelocity <= 20 = addVelocity
   | middlePaddlePointX > ballX  && subVelocity >= 5 = subVelocity
   | otherwise = currentMovementX
   where
        addVelocity = currentMovementX + div (ballX - middlePaddlePointX)10
        middlePaddlePointX = div paddleWidth 2 + x currentPaddle
        subVelocity = currentMovementX -  div ( middlePaddlePointX -ballX) 10 


--After every collision calculate new velocity 
-- ##################################################################
changeMovement :: (MovementDirection, MovementDirection) ->
                  Hit -> Bool -> (MovementDirection, MovementDirection) 
changeMovementAfterCollision:: Hit -> GlobalStatePlay -> Bool -> IO ((), GlobalStatePlay)
changeMovementAfterCollision value s diagonal = do
   let (newVx, newVy) = changeMovement (getMovementTuple s) value diagonal
   let newMovement = Movement {vX=newVx, vY=newVy}
   return ((), s {movement=newMovement})

changeMovement (vX, vY) _ True = switch (vX, vY)
changeMovement (vX, vY) (Hit Right) _ = (PlusDir (unPackMovement vX), vY)
changeMovement (vX, vY) (Hit Left) _ =(NegDir (unPackMovement vX), vY)
changeMovement (vX, vY) (Hit Up) _  =(vX, PlusDir (unPackMovement vY))
changeMovement (vX, vY) (Hit Down) _ = (vX, NegDir (unPackMovement vY))
changeMovement (vX, vY) NoneHit _ = (vX, vY)
 
switch:: (MovementDirection, MovementDirection) ->
         (MovementDirection, MovementDirection) 
switch (PlusDir x, PlusDir y) = (NegDir x, NegDir y) 
switch (PlusDir x, NegDir y) = (NegDir x, PlusDir y)
switch (NegDir x, PlusDir y) = (PlusDir x, NegDir y)
switch (NegDir x, NegDir y) = (PlusDir x, PlusDir y) 
 
 


--Handle unser interaction
-- ##################################################################
    
--Listen to userinput for paddle movement
catchUserKeyEvent:: MVar Block -> Layout -> EventM EKey Bool
catchUserKeyEvent paddleMVar layout = do
   key <- eventKeyName
   let keyValue = glibToString  key
   liftIO $ movePaddle paddleMVar keyValue layout
   return True

-- Move paddle  based an userinput
movePaddle:: MVar Block -> String -> Layout -> IO ()
movePaddle paddleMVar currentDirValue layout =  do
   currentPaddle <- takeMVar paddleMVar
   let (newPaddleX, newPaddleY) = changePaddlePos (x currentPaddle, y currentPaddle) currentDirValue
   let updatePaddle = currentPaddle{x=newPaddleX, y=newPaddleY}
   layoutMove layout (image currentPaddle) newPaddleX newPaddleY
   putMVar paddleMVar updatePaddle

-- Calcuate new coordinates for paddle
changePaddlePos:: (Int, Int) -> String -> (Int, Int)
changePaddlePos (paddleX, paddleY) "Left"  
   | paddleX - speedPaddle  >=0 =   (paddleX - speedPaddle, paddleY)
   | otherwise = (0, paddleY)
changePaddlePos (paddleX, paddleY) "Right"
   | paddleX + speedPaddle + paddleWidth <= diffWindowsWidth =  (paddleX + speedPaddle, paddleY)    
   | otherwise = (diffWindowsWidth-paddleWidth, paddleY)
changePaddlePos (paddleX, paddleY) _  =  (paddleX, paddleY)      



    
        

     



 