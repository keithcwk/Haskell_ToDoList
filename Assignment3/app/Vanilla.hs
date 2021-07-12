----------------------------------------------------------------------------
module Main where

import Lib
import System.IO
import System.Directory
import Data.List
import qualified Data.List.Split as S

-- main
main :: IO()
main = looping

------------------------------Function 1: Show Tasks -------------------------------------
showTasks :: IO()
showTasks = do
            contents <- readFile "todoList.txt"
            if null contents then do
                putStrLn " ---------------------------------- "
                putStrLn "You have finished all tasks!. Start adding a task!"
                putStrLn "Use function [2] in the main menu!"
                putStrLn " ---------------------------------- "

            else do
                let todoTasks = lines contents
                    numTasks = zipWith (\x y -> show x ++ " - " ++ y) [1..] todoTasks
                putStrLn "Here is your To-Do List: "
                putStr $ unlines numTasks

-------------------------------Function 2: Add Tasks ------------------------------------
addTasks :: IO()
addTasks = do
    putStrLn "Please enter a task: "
    todoItem <- getLine
    addTaskGuard todoItem

addTaskGuard :: [Char] -> IO()
addTaskGuard input
    | input <= "" = putStrLn "No task entered. Try Again" >> addTasks
    | otherwise   = putStrLn "\nTask is added successfully!\n" >>
                        appendFile "todoList.txt" (input ++ "\n") >>
                            showTasks


------------------------------- Function 3: Delete Tasks ---------------------------------
deleteTasks :: IO()
deleteTasks = do
    showTasks

    putStrLn "\nWhich line would you like to delete?: (Enter a number within the range shown) "
    userInput <- getLine

    contents <- readFile "todoList.txt"
    let todoTasks = lines contents
        numTasks = length todoTasks

    deleteTasksGuard userInput numTasks

charToInt :: [Char] -> IntDD
charToInt a = read a

deleteTasksGuard :: [Char] -> Int -> IO()
deleteTasksGuard input numTasks
    | charToInt input > numTasks =  putStrLn "Task does not exist. Try Again\n" >> deleteTasks
    | charToInt input < 1 =  putStrLn "Task does not exist. Try Again " >> deleteTasks
    | otherwise = deleteTasksHandler input


deleteTasksHandler :: [Char] -> IO()
deleteTasksHandler input = do
    handle <- openFile "todoList.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    let todoTasks = lines contents
    let number = read input
        newTodoItems = delete (todoTasks !! (number - 1)) todoTasks

    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle

    removeFile "todoList.txt"
    renameFile tempName "todoList.txt"

    showTasks

---------------------------------- Function 4: Delete All Tasks ------------------------------------------
deleteAllTasks :: IO()
deleteAllTasks = do
    showTasks

    putStrLn "\nDelete all todo list items? "
    putStrLn "[1] To Confirm Deletion"
    putStrLn "Warning: This action is non-reversible\n"
    putStrLn "[2] To cancel action\n"
    putStrLn "Enter a number: "
    userInput <- getLine

    deleteAllGuard userInput

deleteAllGuard :: [Char] -> IO()
deleteAllGuard input
    | charToInt input == 1 =  putStrLn "Deleting all tasks..." >> deleteAllHandler
    | charToInt input == 2 =  putStrLn "No items are deleted"
    | otherwise = putStrLn "Enter either 1 or 2!" >> deleteAllTasks

deleteAllHandler :: IO()
deleteAllHandler = do
        handle <- openFile "todoList.txt" ReadMode
        (tempName, tempHandle) <- openTempFile "." "temp"

        hClose handle
        hClose tempHandle

        removeFile "todoList.txt"
        renameFile tempName "todoList.txt"

---------------------------------- Main Menu --------------------------------------------------------------
mainMenu :: IO String
mainMenu = do
    putStrLn "\n\nWelcome to the To-Do List application"
    putStrLn "Please type in the corresponding function number"
    putStrLn "[1] View Todo List"
    putStrLn "[2] Add Todo List Item"
    putStrLn "[3] Delete Todo List Item"
    putStrLn "[4] Delete All Todo List Items"
    putStrLn "[5] End Program"
    getLine

exitMenu :: IO()
exitMenu = putStrLn "Thank you!\nEnter 'looping' to restart!"

processSelection :: String -> IO()
processSelection "1" = showTasks >> mainMenu >>= processSelection
processSelection "2" = addTasks >> mainMenu >>= processSelection
processSelection "3" = deleteTasks >> mainMenu >>= processSelection
processSelection "4" = deleteAllTasks >> mainMenu >>= processSelection
processSelection "5" = exitMenu

looping :: IO()
looping = mainMenu >>= processSelection
-------------------------------------------------------------------------
