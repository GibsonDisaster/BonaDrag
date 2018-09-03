module Utilities where
  import Types

  scale :: Int
  scale = 1

  createSection :: Spec -> Section
  createSection sp sec = Section {
    id = (sId sp)
  }

  outlineFromCorners :: Pos -> Pos -> [Pos]
  outlineFromCorners (x, y) (m, n) = top ++ bottom ++ left ++ right
    where
      left = [(x, p) | p <- [y..n]]
      right = [(m, p) | p <- [y..n]]
      top = drop 1 [(p, y) | p <- [(x-1)..m]]
      bottom = drop 1 [(p, n) | p <- [(x-1)..m]]