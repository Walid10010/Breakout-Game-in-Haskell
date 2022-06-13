
{-# LANGUAGE OverloadedStrings #-}
module PlayModel where 
import Graphics.UI.Gtk hiding(Rectangle)
import Control.Concurrent.MVar
import Server
import PlayConfig 


data GameStatus = Running | Win  | GameOver deriving (Read, Show, Eq)

data GameModus = Normal | Reverse  | ClientModus deriving (Read, Show, Eq)

data Block = Block {image::Image, x::Int, y::Int} 

data Ball  = Ball {imageBall::Image, radius::Int, xBall::Int, yBall::Int}

data GlobalStatePlay = GlobalStatePlay {blockList::[Block], deleteList::[Block],  
                                       paddle::MVar Block,  ball::Ball,
                                       playViewGui::PlayViewGui, movement::Movement, 
                                       clientList::MVar [ClientInfo], 
                                       msgList::MVar [String],
                                       modus::GameModus,
                                       gameStatus::GameStatus}

data PlayViewGui = PlayViewGui {score::Label, textBuffer::TextBuffer, 
                                textView::TextView, layout::Layout, 
                                userNameLabel::Label, sendButton::Button, 
                                userEntry::Entry}

data HitPos = Up| Down| Left| Right deriving (Show, Eq)
data Hit = Hit HitPos | NoneHit deriving (Show, Eq)

instance Ord Hit where
   compare _  (Hit PlayModel.Right) = LT
   compare (Hit PlayModel.Right)  _ = GT
   compare _ (Hit PlayModel.Left) = LT
   compare (Hit PlayModel.Left) _  = GT
   compare _ (Hit Down) = LT
   compare (Hit Down) _  = GT
   compare _ (Hit Up) = LT
   compare (Hit Up) _  = GT
   compare _ _  = EQ




maxIndex:: [Hit] -> (Hit, Int, Int) -> Int
maxIndex [] (_, b, _) = b
maxIndex (x:xs) (a, b, c ) 
   | compare x a == GT = maxIndex xs (x, c , c + 1)
   | otherwise = maxIndex xs (a, b, c + 1)

                    
data MovementDirection = PlusDir Int | NegDir Int deriving (Show)
data Movement = Movement {vX::MovementDirection,  vY::MovementDirection} deriving (Show, Eq)

instance Eq MovementDirection where
  PlusDir _ == PlusDir _ = True
  NegDir _ == NegDir _ = True
  _ == _ = False

unPackMovement:: MovementDirection -> Int
unPackMovement (PlusDir x) = x
unPackMovement (NegDir x) = x

data Rectangle = Rectangle {minX::Int, maxX::Int, minY::Int, maxY::Int}

--Checks if two Rectangles intersects 
rectangleIntersection:: Rectangle -> Rectangle -> Bool
rectangleIntersection rect1 rect2
   | minX rect1 > maxX rect2 || minX rect2 > maxX rect1 = False
   | minY rect1 > maxY rect2 || minY rect2 > maxY rect1 = False
   | otherwise = True

-- Creates a rectangle for a block
createBlockRectangle:: Block -> Rectangle
createBlockRectangle block = Rectangle {minX= x block, maxX=blockXWidth, minY=y block, maxY=blockYHeight}
   where
         blockXWidth = blockWidth + x block
         blockYHeight = blockHeight + y block

-- Creates a rectangle for  Paddle
createPaddleRectangle:: Block -> Rectangle
createPaddleRectangle paddle = Rectangle {minX= x paddle, maxX=paddleXWidth, minY=y paddle, maxY=paddleYHeight}
   where
         paddleXWidth = paddleWidth + x paddle
         paddleYHeight = paddleHeight + y paddle

-- Creates a rectangle for Ball
createBallRectangle:: Ball -> Rectangle
createBallRectangle ball = Rectangle {minX= xBall ball, maxX=ballXWidth, minY=yBall ball, maxY=ballYHeight}
   where
        ballXWidth = ballRadius * 2 + xBall ball
        ballYHeight = ballRadius * 2 + yBall ball

-- Getter for View-Elements

getScoreLabel :: GlobalStatePlay -> Label
getScoreLabel  = score . playViewGui

getTextBuffer :: GlobalStatePlay -> TextBuffer
getTextBuffer = textBuffer . playViewGui

getTextView :: GlobalStatePlay -> TextView
getTextView = textView . playViewGui

getLayout :: GlobalStatePlay -> Layout
getLayout = layout . playViewGui

getSendButton :: GlobalStatePlay -> Button
getSendButton = sendButton . playViewGui

getUserEntry :: GlobalStatePlay -> Entry
getUserEntry = userEntry . playViewGui


-- Getter for Ball 

getImageBall :: GlobalStatePlay -> Image
getImageBall = imageBall . ball

getBallXPos :: GlobalStatePlay -> Int
getBallXPos = xBall . ball

getBallYPos :: GlobalStatePlay -> Int
getBallYPos = yBall . ball

getBallRadius :: GlobalStatePlay -> Int
getBallRadius = radius . ball

getBallYHeight :: GlobalStatePlay -> Int
getBallYHeight = (+(ballRadius*2)) . xBall . ball

getBallXWidth :: GlobalStatePlay -> Int
getBallXWidth = (+(ballRadius*2)) . yBall . ball

getBallPos :: GlobalStatePlay -> (Int, Int)
getBallPos  = \s -> (xBall $ ball s, yBall $ ball s) 

-- Getter for Paddle
getPaddleXWidth :: Block -> Int
getPaddleXWidth = (+paddleWidth) . x

getPaddleYHeight :: Block -> Int
getPaddleYHeight = (+paddleHeight) . y 

-- Getter for Block

getBlockXWidth :: Block -> Int
getBlockXWidth = (+blockWidth) . x

getBlockYHeight:: Block -> Int
getBlockYHeight = (+blockHeight) . y 

-- Getter for Movement 
getMovementTuple:: GlobalStatePlay ->
                   (MovementDirection, MovementDirection)
getMovementTuple currentState = (vX $ movement currentState, vY $ movement currentState)












                          

