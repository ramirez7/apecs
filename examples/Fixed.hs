{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PatternSynonyms       #-}

import Apecs
import Linear (V2 (..))

newtype Position = Position (V2 Double) deriving Show
newtype Velocity = Velocity (V2 Double) deriving Show
data Flying = Flying

-- The 'w' here will be our generated 'World' type. We keep it free here
-- to get around TH restrictions
--
-- We name it 'ActionF' because we use the 'Action' pattern later on
-- to refer to the concrete version that uses 'World'
data Action w = ActionF String (System w ())

makeWorldAndComponentsFixed "World" [''Position, ''Velocity, ''Flying, ''Action]

-- This bidirectional pattern synonym makes it so 'w' is always 'World'
--
-- Without this, you can potentially run into ambiguity due to type inference.
-- (you'd see an error referring to some 'w0' variable)
pattern Action :: String -> (System World ()) -> Action World
pattern Action x y = ActionF x y

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)
  newEntity (Action "screaming" (liftIO $ putStrLn "AAAAAAAAAAAAAAH!!!"))

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  cmap $ \(Position p, Velocity v) -> Position (v+p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

  -- It works!
  cmapM_ $ \(Action tag action) -> do
    liftIO $ putStrLn $ "Doing action " ++ tag ++ "..."
    action

main :: IO ()
main = initWorld >>= runSystem game
