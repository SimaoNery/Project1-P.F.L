import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

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
cities graph = Data.List.nub [city | (startCity, endCity, _) <- graph, city <- [startCity, endCity]]
--1: Use list comprehension to iterate through each tuple in the graph, treating each start and end city as a city, adding them to a list(*has duplicates)
--2: Uses nub to take the result of the list comprehension and remove any duplicates

--Function 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined
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
--1: Base Case -> if the graph is empty the function returns Nothing (no connections exist)
--2: The function checks each road in the graph to see if it connects cityA and cityB. 
--      -If a matching is found returns the distance of the road. 
--      -Otherwise the function recursively calls itself with the remaining roads in the graph.

--Function 4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals: 
--  

--Arguments: 
--  

--Returns:
--  

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined


--Function 5----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
--  Calculate the total distance of a path if all the pairs of cities are directly connected.

--Arguments: 
--  graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.
--  path :: - A list of cities representing the path to evaluate  

--Returns:
--  Maybe Distance - Just the sum of the distances of all consecutive cities if they are all connected. Nothing otherwise

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance graph path = foldl (addDistance graph) (Just 0) (zip path (tail path))
--1: Combines the cities next to each other in tuples using zip (tail will return path without first element)
--2: Foldl will process the list from left to right
--      ->Accumulator starts at 0
--      ->list to fold over has the city tuples
--      ->The binary function to be executed is addDistance


--Helper Function
addDistance :: RoadMap -> Maybe Distance -> (City, City) -> Maybe Distance
addDistance _ Nothing _ = Nothing
addDistance graph (Just acc) (startCity, endCity) = 
    case distance graph startCity endCity of
        Nothing -> Nothing
        Just d  -> Just (acc + d)
--1: If the distance until now is Nothing, it returns Nothing, since there isn't a connected path
--2: If we are staring or the path until now is connected, then we use function distance implemented previously
--      ->If distance returns Nothing (there is not connection between the 2 cities), the function returns nothing
--      ->If there is a path, we add the distance of that path to the accumulator

--Function 6----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

rome :: RoadMap -> [City]
rome = undefined


--Function 7----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined


--Function 8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined


--Function 9----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

travelSales :: RoadMap -> Path
travelSales = undefined


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
