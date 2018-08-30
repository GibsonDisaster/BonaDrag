module Spacing where
  import Types

  {-

    FIRST SECTION IN THE SPACING FUNCTIONS IS ALWAYS THE ONE YOU ARE POSITIONING NOT THE ONE YOU BASE THE POSITIONING OFF OF

    Each section must have a "parent" section

    parent can be either root (no parent since its the first section) or "section" which is passed to the positioning function

    positioning functions:
    root Section (set Section as root)
    Section `below` Section (directly below)
    Section `above` Section (directly above)
    Section `besideR` Section (directly to the right)
    Section `besideL` Section (directly to the left)
    Section `belowN` n Section (N spaces below)
    Section `aboveN` n Section (N spaces above)
    Section `besideRN` n Section (N spaces to the right)
    Section `besideLN` n Section (N spaces to the left)
  -}

  below :: Section a -> Section a -> Section a
  below sec1 sec2 = sec1 { topLeft = (newStartX, newStartY), bottomRight = (newEndX, newEndY) }
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1

  above :: Section a -> Section a -> Section a
  above sec1 sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1
  
  besideR :: Section a -> Section a -> Section a
  besideR sec1 sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1

  besideL :: Section a -> Section a -> Section a
  besideL sec1 sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1

  belowN :: Section a -> Int -> Section a -> Section a
  belowN sec1 n sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1

  aboveN :: Section a -> Int -> Section a -> Section a
  aboveN sec1 n sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1

  besideRN :: Section a -> Int -> Section a -> Section a
  besideRN sec1 n sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1
  besideLN :: Section a -> Int -> Section a -> Section a
  besideLN sec1 n sec2 = sec1
    where
      newStartX = (fst . topLeft) sec2
      newStartY = (snd . topLeft) sec2 + height sec2
      newEndX = (fst . bottomRight) sec2
      newEndY = (snd . bottomRight) sec2 + height sec1