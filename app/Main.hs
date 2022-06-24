module Main where

import Lib
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (isJust, fromMaybe, fromJust, isNothing)
import Rule ( genFstLine, initRule, wolfram )

data Args = Args (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)

defaultArgs :: Args
defaultArgs = Args Nothing Nothing Nothing Nothing Nothing

readInt :: [Char] -> Maybe Int
readInt (c:cs) = case reads (c:cs) of
                [(n, "")] -> Just n
                _ -> Nothing
readInt [] = Nothing

getArg :: [String] -> Args -> Maybe Args
getArg ("--rule":n:z) (Args _ b c d e) = getArg z (Args (readInt n) b c d e)
getArg ("--lines":n:z) (Args a _ c d e) = getArg z (Args a (readInt n) c d e)
getArg ("--move":n:z) (Args a b _ d e) = getArg z (Args a b (readInt n) d e)
getArg ("--start":n:z) (Args a b c _ e) = getArg z (Args a b c (readInt n) e)
getArg ("--window":n:z) (Args a b c d _) = getArg z (Args a b c d (readInt n))
getArg [] args = Just args
getArg _ _ = Nothing

-- printArgs :: Args -> IO()
-- printArgs (Args a b c d e) = do
    -- print a
    -- print b
    -- print c
    -- print d
    -- print e

argsValid :: Maybe Args -> Bool
argsValid (Just (Args a _ _ _ _)) = isJust a
argsValid Nothing = False

getWinFromArgs :: Args -> Int
getWinFromArgs (Args _ _ _ _ win) = fromMaybe 80 win

getRuleFromArgs :: Args -> Int
getRuleFromArgs (Args a _ _ _ _) = fromJust a

getLinesFromArgs :: Args -> Maybe Int
getLinesFromArgs (Args _ b _ _ _) = b

getStartArgs :: Args -> Int
getStartArgs (Args _ _ _ d _) = fromMaybe 0 d

main :: IO()
main = do
    args <- getArgs
    let x = getArg args defaultArgs
    if argsValid x then return () else exitWith (ExitFailure 84)
    let y = fromJust x
    let genNm = fmap (+ getStartArgs y) (getLinesFromArgs y)
    mapM_ (putStrLn . take (getWinFromArgs y)) (drop ( getStartArgs y) (wolfram
        (initRule (getRuleFromArgs y)) 
        (genFstLine (getWinFromArgs y)) (repeat ' ') genNm))

