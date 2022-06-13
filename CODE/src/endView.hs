
module EndView where

import Graphics.UI.Gtk
import LoginConfig
import Control.Monad.IO.Class


createEndView:: String -> IO ()
createEndView msg = do
   initGUI
   window   <- windowNew
   msgLabel <- labelNew $ Just msg
   containerAdd window msgLabel
   windowSetPosition window WinPosCenterAlways
   windowSetDefaultSize window windowsHeight windowsWidth
   widgetShowAll window
   on window deleteEvent  $ liftIO mainQuit >> return False
   mainGUI
   
 

   