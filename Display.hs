{-# LANGUAGE ForeignFunctionInterface #-}

module Display(runWindow) where
  import Prelude hiding (getChar)
  import Foreign.C.Types
  import Data.Char
  import System.Console.ANSI
  import Types
  import Utilities

  {- 
  TODO 
  1) Flow of application (StateT, FRP, etc)
  2) correct drawing of the window
  3) abstract the positioning of windows with stuff like win1 `below` win2 `besideR` win3 etc
  4) abstract everything
  5) why do some topLeft bottomRights not matter like when the section isn't a root. CHANGE THIS
  6) way of composing all the sections together into a window
  -}

  -- Only for use with Windows
  getChar = fmap (chr.fromEnum) c_getch
  foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt

  moveCursor :: (Int, Int) -> IO ()
  moveCursor (x, y) = setCursorPosition y x

  renderSection :: Section a -> IO ()
  renderSection section = do
    let outline = outlineFromCorners (topLeft section) (bottomRight section)
    mapM_ (\c -> moveCursor c >> putChar '#') outline

  runWindow :: Window a -> IO ()
  runWindow window = do
    let secs = sections window
    let newSecs = mapM updateSection secs
    hideCursor
    clearScreen
    setTitle (title window)
    mapM_ renderSection secs
    _ <- getChar
    showCursor
    putStr ""