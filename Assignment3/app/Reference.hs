module Main where

import Lib
import System.IO
import System.Directory
import Data.List

-- appendFile
main :: IO()
main = do
    todoItem <- getLine
    appendFile "todoList.txt" (todoItem ++ "\n")

-- lazy I/O read
-- For txt files the default buffering is line-buffering
-- The smallest part of the file to be read at once is one line
lRead = do
    contents <- readFile "todoList.txt"
    putStr contents

-- non Lazy I/O read
-- BufferMode is a simple enumeration data type and the possible values it can hold
-- hSetBuffering allows the buffering to be controled
nlRead = do
    withFile "todoList.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)

-- delete
flush = do
    handle <- openFile "todoList.txt" ReadMode
    -- takes the path to a temporary directory and a template name for a file
    -- then opens a temp file
    -- returns an I/O action
    -- "." refers to the current directory
    (tempName, tempHandle) <- openTempFile "." "temp"

    -- to do is bound to contents
    contents <- hGetContents handle

    -- split the string into a list of strings, each string one line
    -- add numbers to each element of the list
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

    putStrLn "Here is your To-Do List: "
    -- join list of strings into single newline delimited string with unlines
    -- prints it out to the terminal
    putStr $ unlines numberedTasks
    putStrLn "Which task would you like to delete?"

    -- !! returns an element from a list with some index
    -- delete deletes the first occurence of an element, and returns a list
    numberString <- getLine
    let number = read numberString
        -- bind todoTasks to newTodoItems
        newTodoItems = delete (todoTasks !! number) todoTasks

    -- join todoTasks to newTodoItems into a single string with unlines
    hPutStr tempHandle $ unlines newTodoItems

    -- Close both the original file and temp file
    hClose handle
    hClose tempHandle

    -- removes the old file
    removeFile "todoList.txt"

    -- renaming the temp file with the same name as the old file
    renameFile tempName "todoList.txt"

    -- prints out all the tasks
    putStr "Todo List: \n"
    nlRead
