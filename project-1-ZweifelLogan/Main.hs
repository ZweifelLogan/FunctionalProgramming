module Main where
import Control.Applicative
import Control.Exception
import Data.Maybe
import System.Exit
import Data.Char
import Data.List
import System.Environment
import Control.Monad
import Exercises
import Data.Typeable

main :: IO ()
main =
    do
        args <- getArgs
        case args of
            [] -> fail "Run with one command-line argument"
            "Read":filename:_ ->  -- for problem #2
                do
                    contents <- readFile filename
                    let e = read contents in
                        writeFile "network.txt" $ t2 e
            "Run":filename:sequence:_ -> --for problem #3
                do
                    contents <- readFile filename
                    let e = read contents in
                        print (t3 (read sequence :: [Int]) e)
            "Parallel":filename:_ -> -- for problem #4
                do
                    contents <- readFile filename
                    let e = read contents in
                        writeFile "parallel.txt" $ format4 $ t4 e
            "Sorting":filename:_ ->
                do
                    contents <- readFile filename
                    let e = read contents in
                        print (t5 e)
            "Create":num:_ ->
                do
                    writeFile "parallel.txt" $ format4 $ t6 (read num :: Int)
            _ -> fail "Command not recognized"

