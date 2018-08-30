module Main where
  import Types
  import Display
  import Utilities
  import Spacing

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

  main :: IO ()
  main = runWindow $ Window "Test" [worldSection, statSection, dialogueSection, inventorySection]