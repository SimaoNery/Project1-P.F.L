import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

import System.CPUTime
import Text.Printf

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

--Project Types---------------------------------------------------------------------------------------------------------------------------------------------
type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City, [(City, Distance)])]

--Type Converters----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Function to Convert a RoadMap into an AdjList
--Goals:
--  Convert a representation of roads between cities (RoadMap) into an adjacency list (AdjList), which allows for efficient look-up of neighboring cities and their distances.

convertToAdjList :: RoadMap -> AdjList
convertToAdjList = foldl addConnection []

updateAdjList :: City -> City -> Distance -> AdjList -> AdjList
updateAdjList city neighbor distance adjList =
    case lookup city adjList of
        Just neighbors -> (city, (neighbor, distance) : neighbors) : filter (\(c, _) -> c/= city) adjList
        Nothing -> (city, [(neighbor, distance)]) : adjList

addConnection :: AdjList -> (City, City, Distance) -> AdjList
addConnection adjList (city1, city2, distance) =
    let 
        adjList1 = updateAdjList city1 city2 distance adjList
        adjList2 = updateAdjList city2 city1 distance adjList1
    in adjList2  --entire let...in block will evaluate to the value of adjList2

-- Explanation:
--  1: The conversion starts with an empty adjacency list (`[]`). The foldl function will process each road in the RoadMap, where each road is a tuple of (City, City, Distance)
--  2: For each road connection (city1, city2, distance), the "addConnection" function is called.
--      ->This function first updates the adjacency list for city1 to include city2 as a neighbor along with the distance using "updateAdjList"
--  3: Inside "updateAdjList", it checks if city already exists in the adjList using "lookup"
--      ->If the city is found it adds (neighbor, distance) to the existing list of neighbors and uses "filter" to remove the old entry for city
--      ->If the city is not found a new entry is created with the neighbor and distance
--  4: After updating city1, "addConnection" calls "updateAdjList" again for city2 to add city1 as a neighbor, since the graph is undirected
--  5: Once all roads have been processed, "convertToAdjList" returns the completed adjacency list, which contains each city and a list of its neighbors along with the respective distances

--Complexity:
--  O(E*n)

--Function to Convert an AdjList into a RoadMap
--Goals: 
--  Transform an adjacency list representation of roads between cities (AdjList) back into a standard list of road connections (RoadMap). Each road is represented as a tuple (City, City, Distance).

convertToRoadMap :: AdjList -> RoadMap
convertToRoadMap adjList = removeDuplicates [(city, neighbor, distance) | (city, neighbors) <- adjList, (neighbor, distance) <- neighbors]

removeDuplicates :: RoadMap -> RoadMap
removeDuplicates [] = []
removeDuplicates ((city1, city2, distance):edges) 
    | (city2, city1, distance) `elem` edges = removeDuplicates edges
    | otherwise = (city1, city2, distance) : removeDuplicates edges

--Explanation:
--  1: The "convertToRoadMap" function uses a list comprehension to generate a list of tuples (city, neighbor, distance)
--      ->For each city in the adjacency list, it iterates through its neighbors and constructs a tuple for each connection. This creates a RoadMap that may include duplicate
--  2: The "removeDuplicates" function processes the list of edges to ensure that each connection appears only once. It does this by checking for the reverse edge
--      ->For each edge (city1, city2, distance), it checks if the reverse edge (city2, city1, distance) is present in the remaining list of edges
--          -If the reverse edge exists, it skips adding the current edge
--          -If it does not exist, the edge is included in the final list.
--  3: Once all edges have been processed, the resulting RoadMap contains unique connections between cities, represented as a list of tuples, with no duplicates.

--Complexity:
--  O(E^2)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


--Project Functions-----------------------------------------------------------------------------------------------------------------------------------------------------------

--Function 1----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Goals:
--  Get a list of all the cities in the graph

--Arguments:
--  graph :: RoadMap - A list of tuples where each tuple represents a road with a starting city, and ending city and the distance between them.

--Returns:
--  [City] - A list of unique cities present in the graph given

cities :: RoadMap -> [City]
cities graph = map head (Data.List.group (Data.List.sort [city | (startCity, endCity, _) <- graph, city <- [startCity, endCity]]))

--Explanation:
--  1: Gather all cities in the graph(with duplicates) usign list comprehension
--  2: Order the list with all the cities so duplicates are next to each other using Data.List.sort
--  3: Uses Data.List.group to create a sublists of all identical elements Ex.: [["A", "A"],["B", "B", "B"],["C", "C"]]
--  4: Process each sublist created previously and extracts the first element

--Complexity:
--  O(n log n) -> beacuse of sort, everything else is O(n)


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
pathDistance graph path = 
    let adjList = convertToAdjList graph 
        totalDistance = foldl (addAdjDistance adjList) (Just 0) (zip path (tail path)) 
    in totalDistance

-- Helper function
addAdjDistance :: AdjList -> Maybe Distance -> (City, City) -> Maybe Distance
addAdjDistance _ Nothing _ = Nothing
addAdjDistance adjList (Just acc) (startCity, endCity) =
    case lookup startCity adjList of
        Just neighbors -> case lookup endCity neighbors of
            Nothing -> Nothing 
            Just d  -> Just (acc + d)  
        Nothing -> Nothing  

--Explanation:
--  1: Converts the RoadMap to an Adjacency List for faster lookup
--  2: Combines consecutive cities into pairs 
--  3: Uses foldl to accumulate distances across city pais
--  4: If path is already broken, don't return nothing
--  5: Find neighbors for a city and check if endCity is a neighbor
--        -> Return Nothing if no direct connection
--        -> Add distance if connected
-- 6: Returns Nothing if startCity has no neighbors 

--Complexity:
--  O(E∗N+P∗N)
--  where:
    -- E is the number of edges in the RoadMap
    -- N is the number of cities (unique nodes)
    -- P is the number of cities in the Path
--When the graph has many edges relative to the number of path elements, the adjacency list conversion is dominant, and the complexity is O(E * N)
--If the path length is large compared to the edge count in the graph, O(P * N) becomes more influential


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
-- 

--Arguments: 
-- 

--Returns:
--

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

--Explanation:
--

--Complexity:
--


--Function 8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

--Explanation:
--

--Complexity:
--


--Function 9----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Goals:
-- 

--Arguments: 
-- 

--Returns:
--

travelSales :: RoadMap -> Path
travelSales = undefined

--Explanation:
--

--Complexity:
--


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


--Measurements----------------------------------------------------------------------------------------------------------------------------------------------------
-- Measure the execution time of a given function
measureTime :: (RoadMap -> Path -> Maybe Distance) -> RoadMap -> Path -> IO (Maybe Distance, Double)
measureTime func graph path = do
    start <- getCPUTime
    let result = func graph path
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)  -- Convert from picoseconds to seconds
    return (result, diff)

--main :: IO ()
--main = do
    -- Measure the time for the original implementation
    --(result1, time1) <- measureTime pathDistance gTest1 ["0", "1", "2", "5", "4"]
    --printf "Original pathDistance result: %s, Time taken: %.6f seconds\n" (show result1) time1

    -- Measure the time for the new implementation using AdjList
    --(result2, time2) <- measureTime pathDistance2 gTest1 ["0", "1", "2", "5", "4"]
    --printf "AdjList pathDistance result: %s, Time taken: %.6f seconds\n" (show result2) time2
