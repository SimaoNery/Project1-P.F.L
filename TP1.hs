module TP1 where

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


-- Function 1----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

cities :: RoadMap -> [City]
cities = undefined  -- Modify this line to implement the solution

-- Explanation:
-- 

-- Complexity:
-- 


-- Function 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- Determine whether two cities are directly connected by a road.

-- Arguments: 
-- A RoadMap (list of tuples containing two cities and the distance between them) and two Cities (city1 and city2).

-- Returns:
-- A Boolean value: True if city1 and city2 are directly connected, False otherwise.

areAdjacent :: RoadMap -> City -> City -> Bool 
areAdjacent map city1 city2 =
    any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) map

-- Explanation:
-- This function uses the `any` function to check if there exists any road in the RoadMap that connects city1 to city2 (or vice versa). It iterates over each tuple in the RoadMap and checks for a match.

-- Complexity:
-- The time complexity is O(n), where n is the number of roads in the RoadMap, as it needs to potentially check each road once.


-- Function 3----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined  -- Modify this line to implement the solution

-- Explanation:
-- 

-- Complexity:
-- 


-- Function 4----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- Retrieve a list of cities adjacent to a specified city along with the distances to those cities.

-- Arguments: 
-- A RoadMap (list of tuples containing two cities and the distance between them) and a City (c1).

-- Returns:
-- A list of tuples, where each tuple contains an adjacent City and the corresponding Distance.

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((rc1, rc2, dist):map) c1
    | rc1 == c1 = (rc2, dist) : adjacent map c1
    | rc2 == c1 = (rc1, dist) : adjacent map c1
    | otherwise = adjacent map c1 

-- Explanation:
-- This function recursively traverses the RoadMap, accumulating a list of adjacent cities and their distances if they are connected to the specified city (c1). It checks each road and adds the neighboring city to the result if a match is found.

-- Complexity:
-- The time complexity is O(n), where n is the number of roads in the RoadMap, as it may need to examine each road once.



-- Function 5----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined  -- Modify this line to implement the solution

-- Explanation:
-- 

-- Complexity:
-- 


-- Function 6----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 1. Count the number of adjacent cities for each city in the provided adjacency list.
-- 2. Identify the city or cities with the maximum number of adjacent cities.

-- Arguments: 
-- An AdjList (list of tuples where each tuple contains a city and a list of its neighboring cities with distances).

-- Returns:
-- 1. A list of tuples, where each tuple contains a City and the count of its adjacent cities (from countAdjacent).
-- 2. A list of cities that have the highest number of adjacent cities (from rome).

-- Count Adjacent Function
countAdjacent :: AdjList -> [(City, Int)]
countAdjacent list = map (\(city, adjacent) -> (city, length adjacent)) list

-- Rome Function
rome :: RoadMap -> [City]
rome rmap = 
    let
        list = convertToAdjList rmap                      -- Convert the RoadMap to an adjacency list
        count = countAdjacent list                          -- Count the number of adjacent cities for each city
        maxC = maximum $ map snd count                     -- Find the maximum count of adjacent cities
    in
        [city | (city, count) <- count, count == maxC]    -- Return cities with the maximum count

-- Explanation:
-- The `countAdjacent` function uses `map` to transform each entry in the adjacency list into a tuple containing the city and the count of its neighbors. 
-- The `rome` function first converts the given RoadMap into an adjacency list. It then uses the `countAdjacent` function to determine how many adjacent cities each city has. 
-- After finding the maximum number of adjacent cities, it filters the list to return all cities that share this maximum count.

-- Complexity:
-- The time complexity for `countAdjacent` is O(m), where m is the number of cities in the adjacency list, as it evaluates the length of the neighbor list for each city.
-- The time complexity for `rome` is O(n + m), where n is the number of edges (roads) in the RoadMap, and m is the number of vertices (cities) in the adjacency list, as it involves converting to an adjacency list and counting adjacent cities.


-- Function 7----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined  -- Modify this line to implement the solution

-- Explanation:
-- 

-- Complexity:
-- 


-- Function 8----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- Find all the shortest paths from a starting city to an ending city in a RoadMap.

-- Arguments: 
-- A RoadMap (list of tuples containing two cities and the distances between them), a starting City, and an ending City.

-- Returns:
-- A list of paths (each path is a list of Cities) that represent the shortest paths from the starting city to the ending city.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
    | start == end = [[start]]
    | otherwise = map reverse $ dijkstra [] [(start, 0, [start])] [] Nothing
  where
    -- Convert RoadMap to AdjList
    adjList = convertToAdjList roadmap
    
    -- Dijkstra function with queue processing
    dijkstra :: [(City, Int)] -> [(City, Int, Path)] -> [Path] -> Maybe Int -> [Path]
    dijkstra visited [] results _ = results
    dijkstra visited ((current, dist, path):queue) results minDist
        | current == end =
            case minDist of
                Just d | dist > d -> results
                Just d | dist == d -> dijkstra visited queue (path : results) minDist
                _ -> dijkstra visited queue [path] (Just dist)
        | otherwise = 
            let neighbors = case lookup current adjList of
                    Just neighborsList -> neighborsList  -- If found, use the list of neighbors
                    Nothing -> []
                -- Update queue with neighbors
                newQueue = queue ++ [(nextCity, dist + d, nextCity : path) 
                                     | (nextCity, d) <- neighbors, 
                                       not (any (\(c, d') -> c == nextCity && d' <= dist + d) visited)]
                -- Remove duplicates and sort queue by distance
                sortedQueue = Data.List.sortBy (\(_, d1, _) (_, d2, _) -> compare d1 d2) newQueue
                newVisited = (current, dist) : visited
            in dijkstra newVisited sortedQueue results minDist

-- Explanation:
-- This function implements Dijkstra's algorithm to find all shortest paths from a given start city to an end city. It uses a priority queue to explore paths based on cumulative distance. The algorithm keeps track of visited nodes and paths, returning the shortest paths found.

-- Complexity:
-- The time complexity is O((V + E) log V), where V is the number of vertices (cities) and E is the number of edges (roads). The logarithmic factor arises from the use of a priority queue for managing the shortest path exploration.



-- Function 9----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

travelSales :: RoadMap -> Path
travelSales = undefined  -- Modify this line to implement the solution

-- Explanation:
-- 

-- Complexity:
-- 


-- Function 10----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Goals:
-- 

-- Arguments: 
-- 

-- Returns:
-- 

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined  -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Explanation:
-- 

-- Complexity:
-- 


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]