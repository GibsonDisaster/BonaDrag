module Main where
  import Types
  import Display
  import Utilities
  import Spacing

  {-
    NEW TODO LIST
    [] Get rid of Spec type and just uncurry the fields.
    [] Fix besideR
  -}

  {- World Section -}

  drawWorld :: Section a -> IO ()
  drawWorld section = return ()

  updateWorld :: Section a -> Section a
  updateWorld section = section

  worldSection :: Section Int
  worldSection = Section {
    name = "world",
    width = 10,
    height = 10,
    topLeft = (0 * scale, 0 * scale),
    bottomRight = (10 * scale, 10 * scale),
    sectionState = 3,
    drawSection = drawWorld,
    updateSection = updateWorld
  }

  {- Stat Section -}

  drawStat :: Section a -> IO ()
  drawStat section = return ()

  updateStat :: Section a -> Section a
  updateStat section = section

  statSection :: Section Int
  statSection = Section {
    name = "stats",
    width = 8,
    height = 2,
    topLeft = (10 * scale, 0 * scale),
    bottomRight = (18 * scale, 2 * scale),
    sectionState = 3,
    drawSection = drawStat,
    updateSection = updateStat
  }

  {- Dialogue Section -}

  drawDialogue :: Section a -> IO ()
  drawDialogue section = return ()

  updateDialogue :: Section a -> Section a
  updateDialogue section = section

  dialogueSection :: Section Int
  dialogueSection = Section {
    name = "dialogue",
    width = 8,
    height = 4,
    topLeft = (10 * scale, 2 * scale),
    bottomRight = (18 * scale, 6 * scale),
    sectionState = 3,
    drawSection = drawDialogue,
    updateSection = updateDialogue
  }

  {- Inventory Section -}

  drawInventory :: Section a -> IO ()
  drawInventory section = return ()

  updateInventory :: Section a -> Section a
  updateInventory section = section

  inventorySection :: Section Int
  inventorySection = Section {
    name = "inventory",
    width = 8,
    height = 4,
    topLeft = (10 * scale, 6 * scale),
    bottomRight = (18 * scale, 10 * scale),
    sectionState = 3,
    drawSection = drawInventory,
    updateSection = updateInventory
  }

  -- New System for creating windows

  window :: Window Int
  window = Window "New System" sects
    where
      sects = [root, statS, dialS, invS]
      root = createSectionRoot (Spec "World" 10 10 3 drawWorld updateWorld) (0, 0) (10, 10)
      statS = (createSection (Spec "Stats" 8 2 3 drawStat updateStat) `besideR` root) `leanU` root
      dialS = createSection (Spec "Dialogue" 8 4 3 drawDialogue updateDialogue) `below` statS
      invS = createSection (Spec "Inventory" 8 4 3 drawInventory updateDialogue) `below` dialS

  main :: IO ()
  main = runWindow $ window --Window "Test" [worldSection, statSection, dialogueSection, inventorySection]