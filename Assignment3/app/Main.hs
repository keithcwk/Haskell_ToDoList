----------------------------------------------------------------------------
module Main where

import Lib
import System.IO
import System.Directory
import Data.List

-- main
main :: IO()
main = looping

------------------------------Function 1: Show Tasks -------------------------------------

-- reading and printing the file
showTasks :: IO()
showTasks = do
            -- binds the contents of the txt file to 'contents'
            contents <- readFile "todoList.txt"
            -- checking if the txt file is empty
            if null contents then do
                putStrLn " ---------------------------------- "
                putStrLn "File is empty. Start adding a task!"
                putStrLn "Use function [2] in the main menu!"
                putStrLn " ---------------------------------- "

            else do
                let todoTasks = lines contents
                    numTasks = zipWith (\x y -> show x ++ " - " ++ y) [1..] todoTasks
                putStrLn "Here is your To-Do List: "
                putStr $ unlines numTasks

-------------------------------Function 2: Add Tasks ------------------------------------
-- add tasks
addTasks :: IO()
addTasks = do
    -- prompts user for input
    putStrLn "Please enter a task: "
    todoItem <- getLine

    -- calling addTaskGuard
    -- passing the todoItem to addTaskGuard as a parameter
    addTaskGuard todoItem

-- addTasks guard
addTaskGuard :: [Char] -> IO()
addTaskGuard input
    | input <= "" = putStrLn "No task entered. Try Again" >> addTasks
    | otherwise   = putStrLn "\nTask is added successfully!\n" >>
                        appendFile "todoList.txt" (input ++ "\n") >>
                            showTasks -- can add main menu here


------------------------------- Function 3: Delete Tasks ---------------------------------
-- delete tasks
deleteTasks :: IO()
deleteTasks = do
    -- showing the user list of tasks
    showTasks

    -- obtaining user inputs
    putStrLn "\nWhich line would you like to delete?: "
    putStrLn "(Enter a number within the range shown)"
    userInput <- getLine

    -- obtaining the number of todo list items in the txt file
    contents <- readFile "todoList.txt"
    let todoTasks = lines contents
        numTasks = length todoTasks

    -- delete task checker
    -- takes two parameters:
    -- takes in the user's choice & the number of tasks in the list
    deleteTasksGuard userInput numTasks

-- user input to num
charToInt :: [Char] -> Int
charToInt a = read a

-- delete task guard
deleteTasksGuard :: [Char] -> Int -> IO()
deleteTasksGuard input numTasks
    | charToInt input > numTasks =  putStrLn "Task does not exist. Try Again\n" >> deleteTasks
    | charToInt input < 1 =  putStrLn "Task does not exist. Try Again\n" >> deleteTasks
    | otherwise = putStrLn "Task deleted successfully! \n" >> deleteTasksHandler input

-- deleteTasks handler
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
    -- showing the user list of tasks
    showTasks

    -- obtaining user inputs
    putStrLn "\nDelete all todo list items? "
    putStrLn "[1] To Confirm Deletion"
    putStrLn "Warning: This action is non-reversible\n"
    putStrLn "[2] To cancel action\n"
    putStrLn "Enter a number: "
    userInput <- getLine

    -- call deleteAllGuard
    deleteAllGuard userInput

-- delete task guard
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

----------------------------------Main Menu --------------------------------------------------------------
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
processSelection x = putStrLn "Please enter a number within range!" >>
                        mainMenu >>=
                            processSelection

looping :: IO()
looping = mainMenu >>= processSelection
-----------------------------------------------------------------
