-- AI version of hangman for IO monad practice.

import Data.List
import Data.Ord
import Data.Char
import System.IO

----- Types ---------------------

type Board = [(Letter, Letter)]
type Letter = Char
type Dictionary = [String]
type LetterFreq = (Letter, Int)
type Word = String

----- Types ---------------------


----- Util functions ------------

getDictionary :: IO (Dictionary)
getDictionary = do
  file <- readFile "dictionary.txt"
  return $ lines file

checkWordInDictionary :: Dictionary -> Word -> Bool
checkWordInDictionary dictionary word = (length $ filter ((==) word) dictionary) > 0

----- Util functions ------------

----- Statistical functions -----

firstLetterLengthPair :: Word -> [LetterFreq]
firstLetterLengthPair = map (\x -> (head x, length x)) . group . sort

-- Woorden zoals: sissi hebben 2 letters dus iedere unike letter per woord moet je tellen.
countLetterOccurences :: [LetterFreq] -> LetterFreq
countLetterOccurences = foldr (\(t, x) (_, y) -> (t, y + 1)) ('a', 0)

getLetterFrequencies :: Dictionary -> [LetterFreq]
getLetterFrequencies = 
    reverse 
  . sortBy (comparing $ snd) 
  . map countLetterOccurences
  . groupBy (\(x, _) (y, _) -> x == y) 
  . sortBy (comparing $ fst) 
  . concat 
  . map firstLetterLengthPair

----- Statistical functions -----

----- Cleaning data functions ---

filterDictionaryOnLength :: Int -> Dictionary -> Dictionary
filterDictionaryOnLength n = filter (\x -> length x == n)

filterAlpha :: Dictionary -> Dictionary
filterAlpha = filter (all $ isAlpha)

filterLowerCase :: Dictionary -> Dictionary
filterLowerCase = filter (all $ isLower)

filterByLetters :: [Letter] -> Dictionary -> Dictionary
filterByLetters []     database = database
filterByLetters [x]    database = filter (elem x) database
filterByLetters (x:xs) database = filterByLetters xs $ filter (elem x) database 

filterLetter :: Letter -> Dictionary -> Dictionary
filterLetter x database = filter (not . elem x) database 
----- Cleaning data functions ---

----- Board functions -----------

getLettersFromWord :: Board -> [Letter]
getLettersFromWord board = map snd $ filter (\(_, x) -> if x == '_' || x == ' ' then False else True) board

resetGameBoard :: Word -> Board
resetGameBoard = map (\x -> (x, '_'))

checkLetter :: Board -> Letter -> Bool
checkLetter board letter = 
  any (True == ) $ map (\(char, _) -> char == letter) board
  

putLetterOnBoard :: Board -> Letter -> Board
putLetterOnBoard board letter = 
  map (\(char, under) ->
            if char == letter
            then (char, char) 
            else (char, under)) board

printGameBoard :: Board -> IO ()
printGameBoard = do putStrLn . map snd

----- Board functions -----------

----- AI functions --------------

getBestLetters database board = 
  map fst $ getLetterFrequencies $ filterByLetters availableLetters database
  where availableLetters = getLettersFromWord board

----- AI functions --------------


interactUntil :: (String -> Bool) -> String -> IO (String)
interactUntil p message = do
  putStrLn message
  usrInput <- getLine

  if p usrInput
  then return usrInput
  else do
    putStrLn "Please pick an English word."
    interactUntil p message


-- guess :: Dictionary -> Board -> Letter -> IO ()
-- guess dictionary board letter = do
--   putStrLn "Mmm... Let me think..."
--   putStrLn $ "I'll pick the " ++ [letter]


-- checkLetterIO dictionary board letters = do
--   let letter = head letters
--   guess dictionary board letter
-- 
--   if checkLetter letter
--   then 
--     checkLetterIO dictionary board $ tail letters
--   else 
--     -- filter, pak nieuwe letters en run de functie opnieuw.
--     mapM_ putStrLn $ filterByLetters [letter] dictionary



-- gameLoop :: Dictionary -> Board -> [Letter] -> IO ()
-- gameLoop dictionary board letters = do
--   x <- getBestLetters dictionary board
--   
--   checkLetterIO

  --let letter = head x

  --if checkLetter board letter
  --then do putStrLn "the letter is correct"--putLetterOnBoard board letter
  --else gameLoop dictionary board $ tail x


getLetters board dictionary bestLetters = takeWhile (checkLetter board) bestLetters 

getIncorrectLetter correctLetters mostFreqLetters = 
  last $ take (length correctLetters + 1) mostFreqLetters


reFilter correctLetters incorrectLetter board dictionary errors = 
  if length correctLetters < length board
  then
    reFilter newCorrectLetters newIncorrectLetter board newDictionary (errors + 1)
  else 
    (correctLetters, errors)
  where 
    newDictionary      = filterByLetters correctLetters $ filterLetter incorrectLetter dictionary
    mostFreqLetters    = getBestLetters newDictionary board
    newCorrectLetters  = getLetters board newDictionary mostFreqLetters
    newIncorrectLetter = getIncorrectLetter newCorrectLetters mostFreqLetters


main :: IO ()
main = do
  database <- getDictionary
  usrWord <- interactUntil (checkWordInDictionary database) "Enter a word for me to guess:"

  let filteredDictionary = filterLowerCase $ filterAlpha $ filterDictionaryOnLength (length usrWord) database
  let gameBoard = resetGameBoard usrWord

  printGameBoard gameBoard

  putStrLn "Lets start!"

  let mostFreqLetters = getBestLetters filteredDictionary gameBoard
  let correctLetters = getLetters gameBoard filteredDictionary mostFreqLetters
  let incorrectLetter = getIncorrectLetter correctLetters mostFreqLetters

  let letters = reFilter correctLetters incorrectLetter gameBoard filteredDictionary 0

  -- reFilter correctLetters incorrectLetter gameBoard filteredDictionary 0
  print $ snd $ letters
  print $ fst $ letters



  -- print incorrectLetter

  --mapM_ print correctLetters

  -- mapM_ print filteredFilteredDictionary

 --  if length gameBoard == length correctLetters
 --  else do
 --    let filteredFilteredDictionary = filterByLetters 

