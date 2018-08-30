module Types where

  type Pos = (Int, Int)

  data Section a = Section {
    name :: String,
    width :: Int,
    height :: Int,
    topLeft :: Pos,
    bottomRight :: Pos,
    sectionState :: a,
    drawSection :: (Section a -> IO ()),
    updateSection :: (Section a -> Section a)
  }

  data Window a = Window {
    title :: String,
    sections :: [Section a]
  }

  instance Show (Section a) where
    show (Section i _ _ s e _ _ _) = i ++ " from " ++ show s ++ " to " ++ show e