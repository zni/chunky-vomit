{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Author      :  Matt Godshall
Date        :  2016-08-20
Description :  A simple brainfuck interpreter.
-}

module Main where

import qualified System.Environment as Env
import qualified Data.Array.IArray as IArray
import qualified Data.Array.IO as IOArray

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (ord, chr)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)


type Program = IArray.Array Int Char
type Tape    = IOArray.IOArray Int Int
type ReadPos = Int
type Pointer = Int

type Interp a = ReaderT Program (StateT BFuckState IO) a

newtype Brainfuck a = Brainfuck { brainfuck :: Interp a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadReader Program,
            MonadState BFuckState)

data BFuckState = BFuckState {
  readPos :: ReadPos,
  pointer :: Pointer,
  tape    :: Tape
}

advanceRead :: BFuckState -> BFuckState
advanceRead state = state { readPos = succ . readPos $ state }

moveLeft :: BFuckState -> BFuckState
moveLeft state = state { pointer = pred . pointer $ state }

moveRight :: BFuckState -> BFuckState
moveRight state = state { pointer = succ . pointer $ state }

programSize   :: Program -> Int
programSize p = let (_, size) = IArray.bounds p
                in size

 -- Find matching ']'.
fastForward :: Program -> BFuckState -> BFuckState
fastForward prgm state =
  let read' = readPos state
  in state { readPos = fastForward' prgm (succ read') 1 }

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
rewind :: Program -> BFuckState -> BFuckState
rewind prgm state =
  let read' = readPos state
  in state { readPos = rewind' prgm (pred read') 1 }

rewind' :: Program -> ReadPos -> Int -> ReadPos
rewind' _ pos 0 = succ $ succ pos
rewind' prgm pos seen
  | pos < 0 = error "missing matching '[' brace"
  | otherwise = let ins = prgm IArray.! pos
                in case ins of
                     ']' -> rewind' prgm (pred pos) (succ seen)
                     '[' -> rewind' prgm (pred pos) (pred seen)
                     _   -> rewind' prgm (pred pos) seen

execInstr :: Brainfuck ()
execInstr = do
  program <- ask
  read <- gets readPos
  when (read <= (programSize program)) $
    do tape <- gets tape
       ptr <- gets pointer
       case program IArray.! read of
         -- move tape right
         '>' -> do modify $ moveRight . advanceRead
                   execInstr

         -- move tape left
         '<' -> do modify $ moveLeft . advanceRead
                   execInstr

         -- increment cell at pointer
         '+' -> do cell <- liftIO $ IOArray.readArray tape ptr
                   liftIO $ IOArray.writeArray tape ptr (succ cell)
                   modify advanceRead
                   execInstr

         -- decrement cell at pointer
         '-' -> do cell <- liftIO $ IOArray.readArray tape ptr
                   liftIO $ IOArray.writeArray tape ptr (pred cell)
                   modify advanceRead
                   execInstr

         -- output cell at pointer
         '.' -> do cell <- liftIO $ IOArray.readArray tape ptr
                   liftIO $ putChar (chr $ abs cell)
                   modify advanceRead
                   execInstr
  
         -- input to cell at pointer
         ',' -> do char <- liftIO getChar
                   liftIO $ IOArray.writeArray tape ptr (ord char)
                   modify advanceRead
                   execInstr

         -- jump past ']' if cell == 0
         '[' -> do cell <- liftIO $ IOArray.readArray tape ptr 
                   if cell == 0
                     then do modify $ fastForward program
                             execInstr
                     else do modify advanceRead
                             execInstr

         -- jump back after '[' if cell != 0
         ']' -> do cell <- liftIO $ IOArray.readArray tape ptr
                   if cell /= 0
                     then do modify $ rewind program
                             execInstr
                     else do modify advanceRead
                             execInstr

         -- ignore comments
         _ -> modify advanceRead >> execInstr

exec   :: String -> IO ()
exec p = do
  let memorySize = 30000 :: Int
  tape' <- liftIO $ IOArray.newArray (0, memorySize) 0
  let program = IArray.listArray (0, (length p) - 1) p :: Program
  let state = BFuckState { pointer = 0, readPos = 0, tape = tape' }
  flip evalStateT state . runReaderT (brainfuck execInstr) $ program

readSourceFile :: String -> IO String
readSourceFile = readFile

readSource :: IO String
readSource = getContents

printUsage :: IO ()
printUsage = putStrLn "chunky-vomit <file>"

runInterpreter :: [String] -> IO ()
runInterpreter []       = printUsage
runInterpreter (file:_) = readSourceFile file >>= exec

main :: IO ()
main = Env.getArgs >>= runInterpreter
