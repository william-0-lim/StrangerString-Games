-- Project CPSC312 Haskell
-- STRANGER STRINGS 
-- TaeGyun Lim - 25983157
-- Do Hoon Lee - 26404153

module Project where 
-- To run it, try:
-- ghci
-- :load Project

import System.IO
import System.Exit
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Char  (isSpace, toUpper)
import Data.Maybe (isNothing)

--------------------------------------------------------
-- return True if input contains 'y' or 'Y'
saidYes speech = (elem 'y' speech) || (elem 'Y' speech)

-- ask the user to enter an integer, if failed, retry until success; 
-- returns the input integer
lp_reqInt = do
  input1 <- getLine
  case (readMaybe input1 :: Maybe Int) of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      lp_reqInt
    Just n -> return n

-- ask the user to enter an integer (in a range defined by inclusive min. and max.)
-- if failed, retry until success; returns the input integer
lp_reqIntR imin imax = do
  input1 <- getLine
  case (readMaybe input1 :: Maybe Int) of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      lp_reqIntR imin imax
    Just n -> if ((n<imin) || (n>imax)) 
              then do
                putStrLn ("(input must be within "++show imin++"~"++show imax++")")
                lp_reqIntR imin imax
              else return n

-- ask the user to enter a string, if string matches a certain condition,
-- return the input string, if not, return an error message and ask again
lp_reqStrCond prompt errMsg cond = do
  if (prompt=="") then putStr "" else putStrLn prompt
  input1 <- getLine
  case (cond input1) of
    False -> do
      putStrLn errMsg
      lp_reqStrCond prompt errMsg cond
    True -> return input1

-- find an element from a list that fits a condition. (NOT ROBUST, TARGET MUST EXIST IN LIST)
findElem cond (h:t) =
  if (cond h) then h else findElem cond t

-- remove the first conditionally specified item from a given list, returning the rest
remove1Elem _ [] = []
remove1Elem cond (h:t) = 
  if (cond h) 
  then t
  else h : (remove1Elem cond t)

-- *** Main code ***
d1 = Dungeon 
  (Eli 10 3 [(Item "Key to the dimension" "Use the key to go back to where you are" 10 [(DRooms 10 3)])])
  [
    (Room 10 "\n Welcome stranger...WELCOME TO Stranger State, where you have to play some games to exit or otherwise you will \nbe stuck here forever...\nIMPORTANT TO NOTE that if you do lose at some stage,\nyou might be back to the beignning...\nso be very careful" []),
    (Room 3 "\n Are you ready to take on the challenge brave stranger? *WARNING:CHECKING YOUR LIFE HERE DOES NOT WORK*" 
      [
      (Option "Challenge" "You have decided to fight, wise choice Stranger" [(MoveTo 4)]),
      (Option "Don't Challenge" "You have decided to chicken out... you big chicken" [(DHealth 1), (DHealth 1), (DHealth 1)])
      ]),
    (Room 4 "On your journey, you will have to answer three questions correctly in order to open the gates. You are given 3 life \nand once your life becomes 0, you will die and be stuck here forever, so be very wise Stranger. Good luck. \nHere is the first question of the first game: What is my middle name?" 
      [
      (Option "Haskell" "Well done, you just got lucky on this question" [(MoveTo 5)]),
      (Option "David" "Sorry but you have got the wrong answer, please try again" [(DHealth 1), (MoveTo 10)]),
      (Option "William" "Sorry but you have got the wrong answer, please try again" [(DHealth 1), (MoveTo 10)])
      ]),
    (Room 5 "\n Here is question 2: I'm tall when I'm young, I'm shorty when I'm old. What am I?" 
      [
      (Option "name" "Sorry but you have got the wrong answer, please try again" [(DHealth 1), (MoveTo 10)]),
      (Option "pencil" "Your luck has gotten you this far, but this last question will stop you" [(MoveTo 6)]),
      (Option "age" "Sorry but you have got the wrong answer, please try again" [(DHealth 1), (MoveTo 10)])
      ]),
    (Room 6 "\n LAST QUESTION: what belongs to you but others use it more than you do?" 
      [
      (Option "money" "Sorry but you have got the wrong answer, please try again"  [(DHealth 1), (MoveTo 10)]),
      (Option "dignity" "Sorry but you have got the wrong answer, please try again" [(DHealth 1), (MoveTo 10)]),
      (Option "name" "There is no way you have made it this far...argh, you may move to the next level" [(MoveTo 7)])
      ]),
    (Room 7 "\nThis game will be a bit more difficult... it is a simple ROCK,PAPER, SCISSORS game. \nTo make it harder, a tie is a loss, good luck Stranger" 
      [
      (Option "Play" "You have decided to fight" [(MoveTo 8)]),
      (Option "Don't Play" "You have decided to chicken out" [(MoveTo 10)])
      ]),
    (Room 8 "\n CHOOSE: ROCK, PAPER, SCISSORS!!!" 
      [
      (Option "Rock" "Well done, you won strangely" [(MoveTo 9)]),
      (Option "Paper" "Lucky you, not... just tied it" [(DHealth 1), (MoveTo 10)]),
      (Option "Scissors" "Sorry but you LOST, please try again" [(DHealth 1), (MoveTo 10)])
      ]),
    (Room 9 "\n Second round... CHOOSE: ROCK, PAPER, SCISSORS!!!" 
      [
      (Option "Rock" "Well done, you won strangely. Onto the second round." [(MoveTo 11)]),
      (Option "Paper" "Lucky you, not... just tied it" [(DHealth 1), (MoveTo 10)]),
      (Option "Scissors" "Sorry but you LOST, please try again" [(DHealth 1), (MoveTo 10)])
      ]),
    (Room 11 "\n Last round... CHOOSE: ROCK, PAPER, SCISSORS!!!" 
      [
      (Option "Rock" "Sorry but you LOST, please try again" [(DHealth 1), (MoveTo 10)]),
      (Option "Paper" "How...how did you win this??" [(MoveTo 12)]),
      (Option "Scissors" "Lucky you, not... just tied it hahah" [(DHealth 1), (MoveTo 10)])
      ]),
    (Room 12 "\n Nicely done... to be honest I can not believe you made it this far. Well here is your last game.\nThis depends on your luck. You have one chance.\nI am going to throw a coin, and you are going to pick heads or tails...\n[FLIPS] \nPick either Head or Tails." 
      [
      (Option "Head" "You're luck continues to surprise me." [(MoveTo 13)]),
      (Option "Tail" "Too bad hahah you have to try again from the bottom" [(MoveTo 10)])
      ]),
    (Room 13 "\n Well done player... you have earned my respect. Choose one of the portals to exit from this wonderful game" 
      [
      (Option "Portal 1" "See you later kid" [(DHealth 1),(DHealth 1),(DHealth 1)]),
      (Option "Portal 2" "Opps, did I say that one of the portal takes you back...? hahah" [(MoveTo 10)])
      ])
  ]

enterDungeon = loopDungeon d1 

data Dungeon = Dungeon Eli [Room]            -- "Eli" is the player's avatar
getEli (Dungeon e _) = e
allRooms (Dungeon _ rs) = rs

data Eli = Eli Int Int [Item]                -- first int: position, second int: life
curIndex (Eli i _ _) = i
curLife (Eli _ h _) = h
curInven (Eli _ _ is) = is

data Item = 
     Item [Char] [Char] Int [Effect] -- An item has:
itemName (Item n c ui es) = n        --   A name
itemCons (Item n c ui es) = c        --   Description of what happens when it's used
itemUseI (Item n c ui es) = ui       --   The room index it can be used in, 0 means it can be used anywhere
itemEffs (Item n c ui es) = es       --   List of effects of using the item

putItems [] = putStrLn "You have no items in your inventory. (0: confirm)"
putItems (h:t) = do putStrLn "You also have the following:" 
                    putItems_1 1 (h:t)
putItems_1 _ [] = putStrLn ""
putItems_1 n (h:t) = do
  putStrLn (show n ++ ": " ++ itemName h)
  putItems_1 (n+1) t

data Room = Room Int [Char] [Option]        -- room has: index, description, options in the room
rmIndex (Room i _ _) = i
rmDesc (Room _ d _) = d
rmOpts (Room _ _ os) = os

data Option = Option [Char] [Char] [Effect]    -- option has: 
optDesc (Option d _ _) = d     --   description of the action,
optCons (Option _ c _) = c     --   description of the consequence,
optEffs (Option _ _ es) = es   --   a list of effects

opt0 = Option "Check self." "" []  -- special option to examine the player's avatar

putOpts (h:t) = putOpts_1 0 (h:t)
putOpts_1 _ [] = return()
putOpts_1 n (h:t) = do
  putStrLn (show n ++ ": " ++ optDesc h)
  putOpts_1 (n+1) t

data Effect = DHealth Int       -- changes Eli's health by specified amount
            | DRooms Int Int    -- remove the first indexed room, then change the index of the second to that of the first
            | MoveTo Int        -- move the room as specified by the index


applyEffects d [] = d
applyEffects d (h:t) = do
  let eli = getEli d
  let rooms = allRooms d
  case h of
    DHealth x -> applyEffects (Dungeon (Eli (curIndex eli) (curLife eli - 1) (curInven eli)) rooms) t
    DRooms a b -> applyEffects (Dungeon eli
                  (map (\x -> if (rmIndex x == b) then (Room a (rmDesc x) (rmOpts x)) else x) (remove1Elem (\x -> rmIndex x == a) rooms))) t
    MoveTo i -> applyEffects (Dungeon (Eli i (curLife eli) (curInven eli)) rooms) t


loopDungeon d = do -- *** Main Loop ***
  let eli = getEli d
  if (curLife eli > 0) && (curIndex eli /= 0) 
  then do 
       let curRm = findElem (\rm -> (rmIndex rm) == (curIndex eli)) (allRooms d)
       let opts = rmOpts curRm
       putStrLn (rmDesc curRm ++ "\n\nWhat would you like to do?")
       putOpts (opt0:(rmOpts curRm))
       choice <- lp_reqIntR 0 (length opts)
       putStrLn ("\n")
       if (choice==0) 
       then do
            putStrLn ("You have "++show (curLife eli)++" lives left;")
            let items = curInven eli
            putItems items
            itemChoice <- lp_reqIntR 0 (length items)
            putStrLn("\n")
            if (itemChoice == 0) 
            then (loopDungeon d)
            else do
                 let chosenItem = items !! (itemChoice-1)
                 if (itemUseI chosenItem /= 0) && (itemUseI chosenItem /= curIndex eli)
                 then do
                      putStrLn ""
                      loopDungeon d
                 else do
                      putStrLn (itemCons chosenItem)
                      loopDungeon (applyEffects d (itemEffs chosenItem))
            
       else do
            let chosenOpt = opts !! (choice-1)
            putStrLn (optCons chosenOpt)
            loopDungeon (applyEffects d (optEffs chosenOpt))
  else putStrLn "Your life has become 0. You are now stuck in this shadow realm forever HAHAHA.\nGame Over."