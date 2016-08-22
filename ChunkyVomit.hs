{- |
Author      :  Matt Godshall
Date        :  2016-08-20
Description :  A simple brainfuck interpreter.
-}

module Main where

import qualified System.Environment as Env
import qualified Data.Array.IArray as IArray
import qualified Data.Array.IO as IOArray
import Data.Char (ord, chr)
import Control.Monad (when)


type Program = IArray.Array Int Char
type Tape    = IOArray.IOArray Int Int
type ReadPos = Int
type Pointer = Int

memorySize :: Int
memorySize = 30000

printUsage :: IO ()
printUsage = putStrLn "chunky-vomit <file>"

programSize   :: Program -> Int
programSize p = let (_, size) = IArray.bounds p
                in size

 -- Find matching ']'.
fastForward :: Program -> ReadPos -> ReadPos
fastForward prgm pos = fastForward' prgm (succ pos) 1

fastForward' :: Program -> ReadPos -> Int -> ReadPos
fastForward' _ pos 0 = pos
fastForward' prgm pos seen
  | pos > (programSize prgm) = error "missing matching ']' brace"
  | otherwise = let ins = prgm IArray.! pos
                in case ins of
                     '[' -> fastForward' prgm (succ pos) (succ seen)
                     ']' -> fastForward' prgm (succ pos) (pred seen)
                     _   -> fastForward' prgm (succ pos) seen

 -- Find matching '['.
rewind :: Program -> ReadPos -> ReadPos
rewind prgm pos = rewind' prgm (pred pos) 1

rewind' :: Program -> ReadPos -> Int -> ReadPos
rewind' _ pos 0 = succ $ succ pos
rewind' prgm pos seen
  | pos < 0 = error "missing matching '[' brace"
  | otherwise = let ins = prgm IArray.! pos
                in case ins of
                     ']' -> rewind' prgm (pred pos) (succ seen)
                     '[' -> rewind' prgm (pred pos) (pred seen)
                     _   -> rewind' prgm (pred pos) seen

exec   :: String -> IO ()
exec p = do
  tape <- IOArray.newArray (0, memorySize) 0 :: IO Tape
  exec' (IArray.listArray (0, (length p) - 1) p) 0 tape 0
  where exec' :: Program -> ReadPos -> Tape -> Pointer -> IO ()
        exec' program read tape ptr =
          when (read <= programSize program) $
          case program IArray.! read of
                 -- move tape right
                 '>' -> exec' program (succ read) tape (succ ptr)

                 -- move tape left
                 '<' -> exec' program (succ read) tape (pred ptr)

                 -- increment cell at pointer
                 '+' -> do cell <- IOArray.readArray tape ptr
                           IOArray.writeArray tape ptr (succ cell)
                           exec' program (succ read) tape ptr

                 -- decrement cell at pointer
                 '-' -> do cell <- IOArray.readArray tape ptr
                           IOArray.writeArray tape ptr (pred cell)
                           exec' program (succ read) tape ptr

                 -- output cell at pointer
                 '.' -> do cell <- IOArray.readArray tape ptr
                           putChar (chr $ abs cell)
                           exec' program (succ read) tape ptr

                 -- input to cell at pointer
                 ',' -> do char <- getChar
                           IOArray.writeArray tape ptr (ord char)
                           exec' program (succ read) tape ptr

                 -- jump past ']' if cell == 0
                 '[' -> do cell <- IOArray.readArray tape ptr 
                           if cell == 0
                             then let read' = fastForward program read
                                  in exec' program read' tape ptr
                             else exec' program (succ read) tape ptr

                 -- jump back after '[' if cell != 0
                 ']' -> do cell <- IOArray.readArray tape ptr
                           if cell /= 0
                             then let read' = rewind program read
                                  in exec' program read' tape ptr
                             else exec' program (succ read) tape ptr

                 -- ignore comments
                 _ -> exec' program (succ read) tape ptr


readSourceFile :: String -> IO String
readSourceFile = readFile

readSource :: IO String
readSource = getContents

runInterpreter :: [String] -> IO ()
runInterpreter []       = printUsage
runInterpreter (file:_) = readSourceFile file >>= exec

main :: IO ()
main = Env.getArgs >>= runInterpreter
