module Main where

import TP1

import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)

main :: IO ()
main = loopGraph

loopGraph :: IO ()
loopGraph = do
    -- Choose a graph
    putStrLn "Choose a graph to work with (or 0 to exit):"
    putStrLn "1. gTest1"
    putStrLn "2. gTest2"
    putStrLn "3. gTest3"
    putStr "Enter your choice (0-3): "
    hFlush stdout
    graphChoice <- getLine

    case graphChoice of
        "0" -> putStrLn "Exiting..."
        "1" -> loopFunction gTest1
        "2" -> loopFunction gTest2
        "3" -> loopFunction gTest3
        _   -> do
            putStrLn "Invalid graph choice. Please select 1, 2, or 3."
            loopGraph  -- Retry graph selection

loopFunction :: RoadMap -> IO ()
loopFunction selectedGraph = do
    -- Choose a function
    putStrLn "Choose a function to execute (or 0 to change graph):"
    putStrLn "1. cities"
    putStrLn "2. areAdjacent"
    putStrLn "3. distance"
    putStrLn "4. adjacent"
    putStrLn "5. pathDistance"
    putStrLn "6. rome"
    putStrLn "7. isStronglyConnected"
    putStrLn "8. shortestPath"
    putStrLn "9. travelSales"
    putStr "Enter your choice (0-9): "
    hFlush stdout
    functionChoice <- getLine

    case functionChoice of
        "0" -> loopGraph  -- Go back to graph selection
        "1" -> do
            let result = cities selectedGraph
            print result
            loopFunction selectedGraph  -- Continue with function selection
            
        "2" -> do
            city1 <- getCity "Enter first city: "
            city2 <- getCity "Enter second city: "
            let result = areAdjacent selectedGraph city1 city2
            print result
            loopFunction selectedGraph
            
        "3" -> do
            city1 <- getCity "Enter first city: "
            city2 <- getCity "Enter second city: "
            let result = distance selectedGraph city1 city2
            case result of
                Just dist -> print dist
                Nothing   -> putStrLn "No path found."
            loopFunction selectedGraph
            
        "4" -> do
            city <- getCity "Enter city: "
            let result = adjacent selectedGraph city
            print result
            loopFunction selectedGraph
            
        "5" -> do
            pathInput <- getPath "Enter path (comma-separated cities): "
            let result = pathDistance selectedGraph pathInput
            case result of
                Just dist -> print dist
                Nothing   -> putStrLn "Invalid path."
            loopFunction selectedGraph

        "6" -> do
            let result = rome selectedGraph
            print result
            loopFunction selectedGraph

        "7" -> do
            let result = isStronglyConnected selectedGraph
            print result
            loopFunction selectedGraph

        "8" -> do
            start <- getCity "Enter start city: "
            end <- getCity "Enter end city: "
            let result = shortestPath selectedGraph start end
            print result
            loopFunction selectedGraph

        "9" -> do
            let result = travelSales selectedGraph
            print result
            loopFunction selectedGraph

        _ -> do
            putStrLn "Invalid choice. Please select 0-9."
            loopFunction selectedGraph  -- Retry function selection

-- Helper functions
getCity :: String -> IO City
getCity prompt = do
    putStr prompt
    hFlush stdout
    getLine

getPath :: String -> IO [City]
getPath prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    return $ words $ map (\c -> if c == ',' then ' ' else c) input
