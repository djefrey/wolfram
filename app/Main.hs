module Main where

import Lib
import System.IO
import System.Exit
import System.Environment ( getArgs )

data Generation = Generation { id :: Int, cells :: [Bool], size :: Int }

getId :: Generation -> Int
getId (Generation id _ _) = id

getCells :: Generation -> [Bool]
getCells (Generation _ cells _) = cells

getSize :: Generation -> Int
getSize (Generation _ _ size) = size

startGeneration :: Generation
startGeneration = (Generation 0 [True] 1)

nextGen :: Generation -> (Bool -> Bool -> Bool -> Bool) -> Generation
nextGen (Generation id cells size) rule = (Generation (id + 1) (simulate rule ([False] ++ cells ++ [False]) 0) (size + 2))

simulate :: (Bool -> Bool -> Bool -> Bool) -> [Bool] -> Int -> [Bool]
simulate rule (c:r:xs) 0 = (rule False c r):(simulate rule ([c,r] ++ xs) 1)
simulate rule (c:[]) 0 = [(rule False c False)]
simulate rule (l:[]) pos = [(rule l False False)]
simulate rule (l:c:[]) pos = (rule l c False):(simulate rule [c]  (pos + 1))
simulate rule (l:c:r:xs) pos = (rule l c r):(simulate rule ([c,r] ++ xs) (pos + 1))

getRuleFct :: Conf -> (Bool -> Bool -> Bool -> Bool)
getRuleFct conf = case (getRule conf) of
    Nothing -> rule0
    (Just rule) -> getRuleFromValue rule

getRuleFromValue :: Int -> (Bool -> Bool -> Bool -> Bool)
getRuleFromValue 30 = rule30
getRuleFromValue 90 = rule90
getRuleFromValue 110 = rule110
getRuleFromValue _ = rule0

rule0 :: Bool -> Bool -> Bool -> Bool
rule0 _ _ _ = False

rule30 :: Bool -> Bool -> Bool -> Bool
rule30 False False False = False
rule30 True  False False = True
rule30 False True  False = True
rule30 False False True  = True
rule30 True  True  False = False
rule30 True  False True  = False
rule30 False True  True  = True
rule30 True  True  True  = False

rule90 :: Bool -> Bool -> Bool -> Bool
rule90 False False False = False
rule90 True  False False = True
rule90 False True  False = False
rule90 False False True  = True
rule90 True  True  False = True
rule90 True  False True  = False
rule90 False True  True  = True
rule90 True  True  True  = False

rule110 :: Bool -> Bool -> Bool -> Bool
rule110 False False False = False
rule110 True  False False = False
rule110 False True  False = True
rule110 False False True  = True
rule110 True  True  False = True
rule110 True  False True  = True
rule110 False True  True  = True
rule110 True  True  True  = False

cellToChar :: Bool -> Char
cellToChar True = '*'
cellToChar False = ' '

isGenToBePrint :: Generation -> Conf -> Bool
isGenToBePrint (Generation id _ _) conf = let mayLines = getLines conf in
    case mayLines of
        Nothing -> id >= (getStart conf)
        (Just lines) -> (id >= (getStart conf)) && (id <= ((getStart conf) + lines))

generationToStr :: [Bool] -> Int -> Int -> String
generationToStr _ _ 0 = " "
generationToStr [] _ max = ' ':(generationToStr [] 0 (max - 1))
generationToStr (c:cs) start max
    | start < 0 = ' ':generationToStr (c:cs) (start + 1) (max - 1)
    | start > 0 = generationToStr cs (start - 1) max
    | otherwise = (cellToChar c):(generationToStr cs 0 (max - 1))

removeFinalSpaces :: String -> String
removeFinalSpaces [] = []
removeFinalSpaces (' ':xs) =
    let str = removeFinalSpaces xs
    in case str of
        [] -> []
        _ -> ' ':str
removeFinalSpaces (x:xs) = x:(removeFinalSpaces xs)

getStartPos :: Generation -> Conf -> Int
getStartPos gen conf = (quot ((getSize gen) - (getWindow conf)) 2) - (getMove conf)

printGen :: Generation -> Conf -> IO ()
printGen gen conf =
    if isGenToBePrint gen conf
    then putStrLn (removeFinalSpaces (generationToStr (getCells gen) (getStartPos gen conf) (getWindow conf)))
    else return ()

hasReachEnd :: Int -> Int -> Maybe Int -> Bool
hasReachEnd _ _ Nothing = False
hasReachEnd id start (Just lines) = id >= (start + lines)

run :: Generation -> Conf -> IO ()
run gen conf =
    if not (hasReachEnd (getId gen) (getStart conf) (getLines conf))
    then
        printGen gen conf
        >> run (nextGen gen (getRuleFct conf)) conf
    else
        return ()

wolfram :: Conf -> IO ()
wolfram conf = run startGeneration conf

main :: IO ()
main = do
    args <- getArgs
    startApp (getOpts defaultConf args) wolfram
