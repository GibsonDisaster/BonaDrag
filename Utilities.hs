module Utilities where
  import Types

  scale :: Int
  scale = 1

  createSection :: Spec a -> Section a
  createSection sp = Section {
    name = sName sp,
    width = sWidth sp,
    height = sHeight sp,
    topLeft = (0, 0),
    bottomRight = (0, 0),
    sectionState = sState sp,
    drawSection = sDraw sp,
    updateSection = sUpdate sp
  }

  createSectionRoot :: Spec a -> Pos -> Pos -> Section a
  createSectionRoot sp t b = Section {
    name = sName sp,
    width = sWidth sp,
    height = sHeight sp,
    topLeft = t,
    bottomRight = b,
    sectionState = sState sp,
    drawSection = sDraw sp,
    updateSection = sUpdate sp
  }

  outlineFromCorners :: Pos -> Pos -> [Pos]
  outlineFromCorners (x, y) (m, n) = top ++ bottom ++ left ++ right
    where
      left = [(x, p) | p <- [y..n]]
      right = [(m, p) | p <- [y..n]]
      top = drop 1 [(p, y) | p <- [(x-1)..m]]
      bottom = drop 1 [(p, n) | p <- [(x-1)..m]]