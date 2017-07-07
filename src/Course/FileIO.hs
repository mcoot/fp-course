{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
  args <- getArgs
  case args of
    Nil -> putStrLn "Pls give args :("
    h:._ -> run h

-- main = do
--   args <- getArgs
--   headOr (putStrLn "Pls give args :(") (run <$> args)

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run fname = do
  filesData <- readFile fname
  files <- getFiles (lines filesData)
  printFiles files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . ((<$>) getFile)
--getFiles names = sequence (getFile <$> names)


getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fname =((,) fname) <$> (readFile fname)

-- getFile = lift2 (<$>) (,) readFile

-- getFile fname = do
--     contents <- readFile fname
--     pure (fname, contents)

-- >>= \f -> pure is equivalent to <$>!

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles x = traverse_ (uncurry printFile) x

-- printFiles x = void (sequence ((uncurry printFile) <$> x))

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fname fdata = 
  let list = (("====== " ++ fname) :. fdata :. Nil)
  in traverse_ putStrLn list

-- printFile fname fdata = putStrLn ("============ " ++ fname ++ "\n" ++ fdata)

-- The java way:
-- printFile fname fdata = do
--   putStrLn "============" ++ fname
--   putStrLn fdata
--
-- printFile fname fdata = 
--   putStrLn ("============" ++ fname) *>
--   putStrLn fdata

traverse_ :: Applicative f =>
  (a -> f b) ->
  List a ->
  f ()
traverse_ func list = void (sequence (func <$> list))