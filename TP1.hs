import qualified Data.List as List
import qualified Data.Array as Array
import qualified Data.Bits as Bits

import System.CPUTime
import Text.Printf
import Debug.Trace (trace)


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

--Project Types---------------------------------------------------------------------------------------------------------------------------------------------
type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


--Project Functions-----------------------------------------------------------------------------------------------------------------------------------------------------------

--Function 1----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Goals:
--  Get a list of all the cities in the graph

--Arguments:
--  graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.

--Returns:
--  [City] - A list of unique cities present in the graph given

cities :: RoadMap -> [City]
cities graph = map head (List.group (List.sort [city | (startCity, endCity, _) <- graph, city <- [startCity, endCity]]))

--Explanation:
--  1: Gather all cities in the graph(with duplicates) usign list comprehension
--  2: Order the list with all the cities so duplicates are next to each other using Data.List.sort
--  3: Uses Data.List.group to create a sublists of all identical elements Ex.: [["A", "A"],["B", "B", "B"],["C", "C"]]
--  4: Process each sublist created previously and extracts the first element

--Complexity:
--  O(n log n) -> beacuse of sort, everything else is O(n) - n = number of edges


--Function 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

--Explanation:
--  

--Complexity:
--  


--Function 3---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals: 
--  Finds the distance between two directly connected cities in a graph

--Arguments: 
--  graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.
--  cityA :: City - The starting city
--  cityB : City - The destination city  

--Returns:
--  Maybe Distance - Just Distance if there is a direct connection between the 2 cities
--                 - Nothing otherwise 

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((startCity, endCity, dist) : graph) cityA cityB
    | (startCity == cityA && endCity == cityB) || (startCity == cityB && endCity == cityA) = Just dist
    | otherwise     = distance graph cityA cityB

--Explanation:
--  1: Base Case -> if the graph is empty the function returns Nothing (no connections exist)
--  2: The function checks each road in the graph to see if it connects cityA and cityB. 
--      -If a matching is found returns the distance of the road. 
--      -Otherwise the function recursively calls itself with the remaining roads in the graph.

--Complexity:
--O(n)


--Function 4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals: 
--  

--Arguments: 
--  

--Returns:
--  

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

--Explanation:
--

--Complexity: 
--


--Function 5----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
--  Calculate the total distance of a path if all the pairs of cities are directly connected.

--Arguments: 
--  graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.
--  path :: - A list of cities representing the path to evaluate  

--Returns:
--  Maybe Distance - Just the sum of the distances of all consecutive cities if they are all connected. Nothing otherwise

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance graph path = foldl (addAdjDistance graph) (Just 0) (zip path (tail path))

-- Helper function to add distance between adjacent cities
addAdjDistance :: RoadMap -> Maybe Distance -> (City, City) -> Maybe Distance
addAdjDistance _ Nothing _ = Nothing
addAdjDistance graph (Just acc) (startCity, endCity) =
    case distance graph startCity endCity of
        Nothing -> Nothing
        Just dist  -> Just (acc + dist)

--Explanation:
--  1: Combines consecutive cities into pairs 
--  2: Uses foldl to accumulate distances across city pais
--  3: If path is already broken, don't return nothing
--  4: Use the function "distance" defined earlier to get the distance from a city to another
--  5: If the "distance" returns Nothing we return Nothing. Otherwise we add that distance to the accumulator

--Complexity:
--  O(C * E) where C => number of cities in path
--                 E => number of edges in the graph 


--Function 6----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

rome :: RoadMap -> [City]
rome = undefined

--Explanation:
--

--Complexity:
--


--Function 7----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
--  Indicate if the graph is strongly connected (every city is reachable from every other city)

--Arguments: 
-- graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.

--Returns:
-- Bool - True if is strongly connected, False if not

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected graph =
    let
        citiesList = cities graph
    in
        not (null citiesList) && isReachable (head citiesList) graph citiesList

-- Function to check if all cities are reachable from a start city
isReachable :: City -> RoadMap -> [City] -> Bool
isReachable startCity graph allCities =
    let
        reachableCities = bfs startCity graph
    in
        all (`elem` reachableCities) allCities

-- BFS function to get reachable cities from the graph
bfs :: City -> RoadMap -> [City]
bfs startCity = bfsHelper [startCity] [] --Starting city and RoadMap to return a list with all reachable cities from that point

-- BFS helper function for AdjList
bfsHelper :: [City] -> [City] -> RoadMap -> [City]                              -- list of cities to visit | list of visited cities | adjacency list
bfsHelper [] visitedCities _ = visitedCities                                    --if there are no cities to visit return the list of visited cities
bfsHelper (x:xs) visitedCities graph
    | x `elem` visitedCities = bfsHelper xs visitedCities graph                 --checks if x was already visited, if yes, proceeds with the rest of the cities to visit
    | otherwise =                                                               --if it wasn't visited
        let
            neighbors = [neighbor | (city, neighbor, _) <- graph, city == x]    --find neighbors of the current city by looking through the RoadMap.
            newVisitedCities = x : visitedCities                                --update the list of cities that were visited
            newQueue = xs ++ filter (`notElem` newVisitedCities) neighbors      --update the list of cities to be visited with the neighbors of the current city that are not in visitedCities
        in
            bfsHelper newQueue newVisitedCities graph                            --continue the BFS until the queue is empty, all cities are visited    

--Explanation:
--  1: Gets a list of all unique cities in the graph and checks if the list is not null and then will guarantee that they are all reachable
--  2: Will do a bfs from the first city in the list of unique cities to get a list with all the reachable cities from that point
--  3: Will check if for every element in the list of unique cities is in the list of reachable cities using `elem`
--  4: Since the graph is undirected, we only need to know if there are separate components, because otherwise the graph will always be strongly connected 

--Complexity:
--  O(C + E) where C => number of cities in path
--                 E => number of edges in the graph  


-

--Function 10----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTestBigGraph :: RoadMap
gTestBigGraph = [
    ("0", "1", 1), ("0", "2", 2), ("0", "3", 3), ("0", "4", 4), ("0", "5", 5),
    ("1", "2", 6), ("1", "3", 7), ("1", "4", 8), ("1", "5", 9), ("1", "6", 10),
    ("2", "3", 11), ("2", "4", 12), ("2", "5", 13), ("2", "6", 14), ("2", "7", 15),
    ("3", "4", 16), ("3", "5", 17), ("3", "6", 18), ("3", "7", 19), ("3", "8", 20),
    ("4", "5", 21), ("4", "6", 22), ("4", "7", 23), ("4", "8", 24), ("4", "9", 25),
    ("5", "6", 26), ("5", "7", 27), ("5", "8", 28), ("5", "9", 29), ("5", "10", 30),
    ("6", "7", 31), ("6", "8", 32), ("6", "9", 33), ("6", "10", 34), ("6", "11", 35),
    ("7", "8", 36), ("7", "9", 37), ("7", "10", 38), ("7", "11", 39), ("7", "12", 40),
    ("8", "9", 41), ("8", "10", 42), ("8", "11", 43), ("8", "12", 44), ("8", "13", 45),
    ("9", "10", 46), ("9", "11", 47), ("9", "12", 48), ("9", "13", 49), ("10", "11", 50),
    ("10", "12", 51), ("10", "13", 52), ("11", "12", 53), ("11", "13", 54), ("12", "13", 55),

    ("10", "14", 10), ("11", "15", 11), ("12", "16", 12), ("13", "17", 13), ("14", "15", 14),
    ("15", "16", 15), ("16", "17", 16), ("17", "18", 17), ("18", "19", 18), ("19", "20", 19),
    ("20", "21", 20), ("21", "22", 21), ("22", "23", 22), ("23", "24", 23), ("24", "25", 24),
    ("25", "26", 25), ("26", "27", 26), ("27", "28", 27), ("28", "29", 28), ("29", "30", 29),
    ("30", "31", 30), ("31", "32", 31), ("32", "33", 32), ("33", "34", 33), ("34", "35", 34),
    ("35", "36", 35), ("36", "37", 36), ("37", "38", 37), ("38", "39", 38), ("39", "40", 39),
    ("40", "41", 40), ("41", "42", 41), ("42", "43", 42), ("43", "44", 43), ("44", "45", 44),
    ("45", "46", 45), ("46", "47", 46), ("47", "48", 47), ("48", "49", 48), ("49", "50", 49),

    ("50", "51", 10), ("51", "52", 11), ("52", "53", 12), ("53", "54", 13), ("54", "55", 14),
    ("55", "56", 15), ("56", "57", 16), ("57", "58", 17), ("58", "59", 18), ("59", "60", 19),
    ("60", "61", 20), ("61", "62", 21), ("62", "63", 22), ("63", "64", 23), ("64", "65", 24),
    ("65", "66", 25), ("66", "67", 26), ("67", "68", 27), ("68", "69", 28), ("69", "70", 29),
    ("70", "71", 30), ("71", "72", 31), ("72", "73", 32), ("73", "74", 33), ("74", "75", 34),
    ("75", "76", 35), ("76", "77", 36), ("77", "78", 37), ("78", "79", 38), ("79", "80", 39),
    ("80", "81", 40), ("81", "82", 41), ("82", "83", 42), ("83", "84", 43), ("84", "85", 44),
    ("85", "86", 45), ("86", "87", 46), ("87", "88", 47), ("88", "89", 48), ("89", "90", 49),
    ("90", "91", 50), ("91", "92", 51), ("92", "93", 52), ("93", "94", 53), ("94", "95", 54),
    ("95", "96", 55), ("96", "97", 56), ("97", "98", 57), ("98", "99", 58), ("99", "0", 59)]--, ("101", "102", 10)]