import System.IO
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
 
--main function
main = do
        putStrLn "Welcome to Money Budgeter!"
        fileExist <- checkFile
        if fileExist == True
        then do
                contents <- readFile "budget.txt"
                resetBudget "budget.txt" contents --clear the budget record if it's a new week
 
                if contents == ""
                        then do
                                initBudget "budget.txt"
                                mainLoop True contents
                        else do
                                let currentStateInfo = last (lines contents)
 
                                printStats currentStateInfo
                                mainLoop True contents
 
        else do
                putStrLn "Please create a text file MYBUDGET.txt within the same directory."
                endBudgeter
 
--checks for existence of budget.txt
checkFile :: IO Bool
checkFile = do
        dfe <- doesFileExist "budget.txt"
        return $ dfe
 
--prints a farewell message
endBudgeter :: IO ()
endBudgeter = do
        putStrLn ""
        putStrLn "Have a thrifty day!"
 
--iterates until user decides to exit the program
mainLoop :: Bool -> String -> IO ()
mainLoop continue contents = do
                askAction
                response <- getLine
--              action <- processResponse response
 
                if response == "1"
                        then do
                                addExpense contents "budget.txt"
                                mainLoop True contents
                else if response == "2"
                        then do
                                viewSpending contents
                                mainLoop True contents
                else if response == "3"
                        then endBudgeter
                else do
                        putStrLn "invalid response"
                        mainLoop True contents
 
--print current budget, amount spent, and amount remaining
printStats :: String -> IO ()
printStats currentStateInfo = do
        let [goal, spent, left] = words currentStateInfo
        putStr "Budget goal: "
        putStrLn goal
        putStr "Amount spent this week: "
        putStrLn spent
        putStr "Amount remaining for this week: "
        putStrLn left
 
--prompt user for action to take
askAction :: IO ()
askAction = do
        putStrLn "What would you like to do? Please enter either: "
        putStrLn "1 to add an expense"
        putStrLn "2 to view all past spending"
        putStrLn "3 to quit"
 
--unused function
processResponse :: String -> IO String
processResponse response
        | response == "1" = return "1"
        | response == "2" = return "2"
        | response == "3" = return "3"
        | otherwise = return "error"
 
--read string as integer
rInt :: String -> Int
rInt = read
 
--adds an expense to budget.txt
addExpense :: String -> String -> IO ()
addExpense contents file = do
        let oldInfo = words (last $ lines contents)
        putStrLn "What was the date of your expense? (mm dd)"
        date <- getLine
 
        putStrLn "What was the amount of your expense? (do not include dollar signs)"
        expense <- getLine
        let newEntry = "\n " ++ date ++ " " ++ expense
        writeFile file newEntry
        let newSpent = (rInt expense) + (rInt (head $ tail oldInfo))
        let newRemaining = (rInt $ last oldInfo) - (rInt expense)
        let newInfo = (head oldInfo) ++ (show newSpent) ++ (show newRemaining)
        let newcontents = contents ++ "\n" ++ date ++ " " ++ expense ++ "\n" ++ newInfo
        writeFile file newcontents
        if newRemaining <= 0
                then do
                        putStrLn "You went over budget, goddamnit."
                else do
                        putStrLn "Still under budget, huzzah!"
 
--initialize this week's budget
--note that the week automatically ends after Saturday, 11:59 pm
initBudget :: String -> IO ()
initBudget file = do
        putStrLn "It looks like you haven't set up a budget for this week yet."
        putStrLn "What is your budget for this week? Omit dollar signs and periods."
        budget <- getLine
        putStrLn "What is today's date? (mm dd)"
        date <- getLine
        let line = date ++ "\n" ++ budget ++ "0" ++ budget
        writeFile file line
        let output = "You have " ++ budget ++ " for this week."
        putStrLn output
        putStrLn "Alright, you're all set! Happy saving!"
 
--checks if it is a new week; if so, automatically clears the records for the week
--timezone code from https://techoverflow.net/blog/2014/06/13/get-current-year-month-day-in-haskell/
resetBudget :: String -> String -> IO ()
resetBudget file contents = do
        let startDate = read (head $ lines contents)
        let startMonth = read (head $ words startDate)
        let startDay = read (last $ words startDate)
        --get current time
        now <- getCurrentTime
        timezone <- getCurrentTimeZone
        let zoneNow = utcToLocalTime timezone now
        let (year, month, day) = toGregorian $ localDay zoneNow
 
        --admittedly this code assumes you use the MoneyBudgeter actively
        if startMonth == month
                then do
                        if day - startDay >= 7 then do writeFile file ""
                                else return ()
                        --very lazy way of getting a week that is part of two different months because I am out of time
                else do
                        if day - startDay >= 7 then do writeFile file ""
                        else do
                                return ()
