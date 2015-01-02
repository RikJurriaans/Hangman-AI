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

filterWordlistOnLength :: Int -> Dictionary -> Dictionary
filterWordlistOnLength n = filter (\x -> length x == n)

filterAlpha :: Dictionary -> Dictionary
filterAlpha = filter (all $ isAlpha)

filterLowerCase :: Dictionary -> Dictionary
filterLowerCase = filter (all $ isLower)

filterByLetters :: [Letter] -> Dictionary -> Dictionary
filterByLetters []     database = database
filterByLetters [x]    database = filter (elem x) database
filterByLetters (x:xs) database = filterByLetters xs $ filter (elem x) database 

----- Cleaning data functions ---

----- Board functions -----------

getLettersFromWord :: Board -> [Letter]
getLettersFromWord board = map snd $ filter (\(_, x) -> if x == '_' || x == ' ' then False else True) board

resetGameBoard :: Word -> Board
resetGameBoard = map (\x -> (x, '_'))

checkLetter :: Board -> Letter -> Board
checkLetter board letter = 
  map (\(char, under) ->
            if char == letter
            then (char, char) 
            else (char, under)) board

printGameBoard :: Board -> IO ()
printGameBoard = do putStrLn . map snd

----- Board functions -----------

----- AI functions --------------

guess :: Dictionary -> Board -> IO (Letter, [LetterFreq])
guess database word = do
  let availableLetters = getLettersFromWord word
  let letterFreqencies = getLetterFrequencies $ filterByLetters availableLetters database
  let char = fst $ head letterFreqencies

  putStrLn "Mmm... Let me think..."
  putStrLn $ "I'll pick the " ++ [char]

  return (char, tail letterFreqencies)

----- AI functions --------------


interactUntil :: (String -> Bool) -> String -> IO (String)
interactUntil p message = do
  putStrLn message
  usrInput <- getLine

  if p usrInput
  then return usrInput
  else do
    putStrLn "Please pick an english word."
    interactUntil p message
  


main :: IO ()
main = do
  database <- getDictionary
  usrWord <- interactUntil (checkWordInDictionary database) "Enter a word for me to guess:"

  let filteredWordlist = filterLowerCase $ filterAlpha $ filterWordlistOnLength (length usrWord) database
  let gameBoard = resetGameBoard usrWord

  printGameBoard gameBoard

  putStrLn "Lets start!"

  firstGuess <- guess filteredWordlist gameBoard

  let newGameBoard = checkLetter gameBoard $ fst firstGuess

  printGameBoard newGameBoard


