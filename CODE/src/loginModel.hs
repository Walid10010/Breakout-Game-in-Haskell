
module LoginModel where
import Graphics.UI.Gtk 
import Text.Read

--GlobalState for LoginScreen
type Location = (Int, Int, String)
data GlobalState = GlobalState {picImageList::[Image], currentLocation::Location, interactiveGui::LoginGui, handlerId::HandlerId}

-- Interactive Elements in LoginScreen
data LoginGui = LoginGui {adjustment::Adjustment, nameEntry::Entry, 
                          portEntry::Entry, gameButton::Button, 
                          watchButton::Button, userNameError::Label,
                          portError::Label, ipEntry::Entry, 
                          sliderValue::Int, normalButton::RadioButton, 
                          reverseButton::RadioButton,
                          layout::Layout} 
 

--getter for LoginGui Elements

getSlider :: GlobalState -> Adjustment
getSlider = adjustment . interactiveGui

getUserNameEntry :: GlobalState -> Entry
getUserNameEntry = nameEntry . interactiveGui

getPortEntry :: GlobalState -> Entry
getPortEntry = portEntry . interactiveGui

getIpEntry :: GlobalState -> Entry
getIpEntry = ipEntry . interactiveGui

getGameButton :: GlobalState -> Button
getGameButton = gameButton . interactiveGui

getWatchButton :: GlobalState -> Button
getWatchButton = watchButton . interactiveGui

getUserNameErrorLabel :: GlobalState -> Label
getUserNameErrorLabel = userNameError . interactiveGui

getPortErrorLabel :: GlobalState -> Label
getPortErrorLabel = portError . interactiveGui

getAdjustment :: GlobalState -> Adjustment
getAdjustment = adjustment . interactiveGui 

getNormalButton :: GlobalState -> RadioButton
getNormalButton = normalButton . interactiveGui

getReverseButton :: GlobalState -> RadioButton
getReverseButton = reverseButton . interactiveGui 

getLayout :: GlobalState -> Layout
getLayout = layout . interactiveGui


checkValidPort:: String -> Either String String
checkValidPort portNr = 
               if checkLength then
                  case value of 
                    Just _ -> Right portNr
                    _  -> Left "Parse Error "
               else 
                  Left "Port Digits must be 4"
                where 
                    checkLength = length portNr == 4
                    value = readMaybe portNr:: Maybe Int


checkUserName::String -> Either String String
checkUserName username = 
                  if length username < 3 then Left tooShortMsg
                  else Right username
                  where
                     tooShortMsg = "UserName to short!"   
         