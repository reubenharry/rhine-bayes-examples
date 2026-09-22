module UserInput where

data UserInput = UserInput
  { mouseX :: Float
  , mouseY :: Float
  }
  deriving (Eq, Show)

noInput :: UserInput
noInput = UserInput 0 0
