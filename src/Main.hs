{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

--import Graphics.Vty.Widgets.All
--import Data.Text as T
import Control.Monad.IO.Class
import UI.NCurses
import Data.IORef
import qualified Graphics.Vty.Widgets.TextZipper as TZ
import Data.Text

blankState :: MyState
blankState = TZ.textZipper [""]
    
type MyState = TZ.TextZipper Text

main :: IO ()
main = do
    stateRef <- newIORef blankState
    password <- runCurses $ do
        setEcho False
        w <- defaultWindow
        updateWindow w $ do
            moveCursor 1 10
            drawString "♥ Hello world! ♥"
            moveCursor 0 0
        render
        loop w stateRef
    print password

loop :: Window -> IORef MyState -> Curses [Text]
loop w stateRef = do
    mev <- getEvent w (Just 16)
    case mev of
        Nothing -> loop'
        Just ev -> handleEvent ev
    where
    loop' = loop w stateRef
    handleEvent ev
        | isKeyLeftArrow    ev = stepLeft  w >> render >> loop'
        | isKeyRightArrow   ev = stepRight w >> render >> loop'
        | isKeyUpArrow      ev = stepUp    w >> render >> loop'
        | isKeyDownArrow    ev = stepDown  w >> render >> loop'
        | isKeyBackspace    ev = loop'
        | isLetter          ev = do
            let EventCharacter char = ev
            drawChar w char
            liftIO $ addChar stateRef char
            render
            loop'
        | isKeySpace        ev = do
            updateKeySpace w stateRef
            loop'
        | not $ isExitEvent ev = do
            -- updateWindow w . drawString . show $ ev
            -- render
            loop'
        | otherwise            = liftIO $ return . TZ.getText =<< readIORef stateRef
    isLetter ev     = elem ev $ fmap EventCharacter ['a'..'z'] ++ fmap EventCharacter ['A'..'Z']
    isKeyLeftArrow  = (== EventSpecialKey KeyLeftArrow)
    isKeyRightArrow = (== EventSpecialKey KeyRightArrow)
    isKeyUpArrow    = (== EventSpecialKey KeyUpArrow)
    isKeyDownArrow  = (== EventSpecialKey KeyDownArrow)
    isKeyBackspace  = (== EventSpecialKey KeyBackspace)
    isKeySpace      = (== EventCharacter ' ')

updateKeySpace :: Window -> IORef MyState -> Curses ()
updateKeySpace _ {- window -} stateRef = do
    oldState <- liftIO $ readIORef stateRef
    -- let oldCursorPos = TZ.cursorPosition oldState
    -- let oldLineLengths = TZ.lineLengths oldState
    liftIO . writeIORef stateRef . TZ.breakLine $ oldState

isExitEvent :: Event -> Bool
isExitEvent ev = EventCharacter '\n' == ev

moveRelative :: Window -> Integer -> Integer -> Curses ()
moveRelative w x y = do
    (col, row) <- getCursor w
    updateWindow w $ moveCursor (col + y) (row + x)

stepLeft :: Window -> Curses ()
stepLeft  w = moveRelative w (-1)   0 

stepRight :: Window -> Curses ()
stepRight  w = moveRelative w 1   0 

stepUp :: Window -> Curses ()
stepUp w = moveRelative w 0   (-1)

stepDown :: Window -> Curses ()
stepDown w = moveRelative w 0   1 

drawChar :: Window -> Char -> Curses ()
drawChar w char = updateWindow w $ drawString [char]

addChar :: IORef MyState -> Char -> IO ()
addChar stateRef char = do
    state <- readIORef stateRef
    let newState = TZ.insertChar char state
    writeIORef stateRef newState

