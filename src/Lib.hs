module Lib
    (
    Conf, getRule, getLines, getStart, getWindow, getMove,
    defaultConf, startApp, getOpts,
    substring,
    ) where

import System.IO
import System.Exit
import Text.Read

data Conf = Conf {
        rule :: Maybe Int,
        start :: Int,
        line :: Maybe Int,
        window :: Int,
        move :: Int
    }
    deriving (Show)

defaultConf :: Conf
defaultConf = Conf Nothing 0 Nothing 80 0

invalidRule :: Maybe Int -> Bool
invalidRule Nothing  = True
invalidRule (Just rule) = not ((rule == 30) || (rule == 90) || (rule == 110))

invalidValues :: Conf -> Bool
invalidValues (Conf _ start Nothing window _) =
    (start < 0) || (window <= 0)
invalidValues (Conf _ start (Just lines) window _) =
    (start < 0) || (lines < 0) || (window <= 0)

startApp :: Maybe Conf -> (Conf -> IO ()) -> IO ()
startApp Nothing _ = exitWith (ExitFailure 84)
startApp (Just conf) fct =
    if (invalidRule (getRule conf)) || (invalidValues conf)
        then exitWith (ExitFailure 84)
        else fct conf

setRuleFromStr, setStartFromStr :: Conf  -> String -> [String] -> Maybe Conf
setLinesFromStr, setWindowFromStr :: Conf -> String -> [String] -> Maybe Conf
setMoveFromStr :: Conf -> String -> [String] -> Maybe Conf
setRuleFromStr conf str strs =
    let mVal = readMaybe str :: Maybe Int in case mVal of
        Nothing -> Nothing
        (Just val) -> getOpts conf{rule=(Just val)} strs
setStartFromStr conf str strs =
    let mVal = readMaybe str :: Maybe Int in case mVal of
        Nothing -> Nothing
        (Just val) -> getOpts conf{start=val} strs
setLinesFromStr conf str strs =
    let mVal = readMaybe str :: Maybe Int in case mVal of
        Nothing -> Nothing
        (Just val) -> getOpts conf{line=(Just val)} strs
setWindowFromStr conf str strs =
    let mVal = readMaybe str :: Maybe Int in case mVal of
        Nothing -> Nothing
        (Just val) -> getOpts conf{window=val} strs
setMoveFromStr conf str strs =
    let mVal = readMaybe str :: Maybe Int in case mVal of
        Nothing -> Nothing
        (Just val) -> getOpts conf{move=val} strs

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf ("--rule":str:strs) = setRuleFromStr conf str strs
getOpts conf ("--start":str:strs) = setStartFromStr conf str strs
getOpts conf ("--lines":str:strs) = setLinesFromStr conf str strs
getOpts conf ("--window":str:strs) = setWindowFromStr conf str strs
getOpts conf ("--move":str:strs) = setMoveFromStr conf str strs
getOpts conf [] = (Just conf)
getOpts _ _ = Nothing

getRule, getLines :: Conf -> Maybe Int
getStart, getWindow, getMove :: Conf -> Int
getRule (Conf rule _ _ _ _) = rule
getStart (Conf _ start _ _ _) = start
getLines (Conf _ _ lines _ _) = lines
getWindow (Conf _ _ _ window _) = window
getMove (Conf _ _ _ _ move) = move

substring :: String -> Int -> Int -> String
substring _ _ 0 = ""
substring [] start size = ' ':(substring [] start (size - 1))
substring (c:str) start size
    | start <= 0 = c:(substring str (start - 1) (size - 1))
    | otherwise = substring str (start - 1) size