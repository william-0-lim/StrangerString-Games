module DBALLZ where

import Data.Maybe (isNothing)
import RPS

data MoveD = Charge | Attack | Block | Special
  deriving (Show, Eq)

type Energy = Int

data OutcomeD = WinD | LossD | Charged | AMissed | SMissed | Blocked
  deriving (Show, Eq)


getOutcomeD :: MoveD -> MoveD -> OutcomeD

getOutcomeD Attack computer
   | computer == Charge = WinD
   | computer == Special = LossD
   | otherwise = AMissed

getOutcomeD Charge computer
   | computer == Charge = Charged
   | computer == Block = Charged
   | otherwise = LossD

getOutcomeD Block computer
   | computer == Special = LossD
   | otherwise = Blocked

getOutcomeD Special computer
   | computer == Special = SMissed
   | otherwise = WinD


-- Prompt the user to input a move
getUserInputD :: IO String
getUserInputD = do
  putStrLn ""
  putStrLn "What move will you pick? 1. Charge 2. Attack 3. Block 4. Special: "
  getLine

-- Parse input and maybe return a move
parseInputD :: String -> Maybe MoveD
parseInputD string = case string of
  "1" -> Just Charge
  "2" -> Just Attack
  "3" -> Just Block
  "4" -> Just Special
  _ -> Nothing

-- Prompt the user until they enter a valid move
getUserMoveD :: Energy -> IO MoveD
getUserMoveD userEnergy = do
    maybeMove <- parseInputD <$> getUserInputD
    case maybeMove of
      Nothing -> do
        putStrLn "Please enter a valid move."
        getUserMoveD userEnergy
      Just Charge -> do
        return Charge
      Just Attack -> 
        if userEnergy >= 1
        then return Attack 
        else do 
            putStrLn "You don't have enough energy for attack."
            getUserMoveD userEnergy
      Just Special -> 
        if userEnergy >= 5
        then return Special
        else do
            putStrLn "You don't have enough energy for special attack."
            getUserMoveD userEnergy
      Just Block -> return Block

runGameD :: Energy -> StrangerState -> IO StrangerState
runGameD userEnergy life = 
  do
    putStrLn ("You have "++(show userEnergy)++" energy left.")
    userMove <- getUserMoveD userEnergy
    let outcome = getOutcomeD userMove Block
    case outcome of
        WinD -> do
            putStrLn "You won the fight."
            return life
        LossD -> do
            putStrLn "You lost the fight."
            return (life-1)
        Charged -> do
            putStrLn "You have charged an energy."
            runGameD (userEnergy + 1) life
        AMissed -> do
            putStrLn "Your attack has missed."
            runGameD (userEnergy - 1) life
        SMissed -> do
            putStrLn "Your special attack has missed."
            runGameD (userEnergy - 5) life
        Blocked -> do
            putStrLn "You blocked."
            runGameD userEnergy life

mainD = runGameD 0 1