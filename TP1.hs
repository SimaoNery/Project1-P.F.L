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
--  O(n log n) -> beacuse of sort, everything else is O(n) - n = number of edges


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
-- The goal of `travelSales` is to solve the Traveling Salesman Problem (TSP) using dynamic programming with memoization. 
-- Given a map of cities and roads, it finds the shortest possible path that visits each city exactly once and returns to the starting city.

-- Arguments: 
-- `graph` (of type `RoadMap`): A graph structure that contains cities and roads, where `cities graph` lists all cities and `distance graph city1 city2` gives the distance between two cities (if they are connected).

-- Returns:
-- `Path`: A list of cities representing the optimal path that minimizes the total travel distance while visiting each city exactly once and returning to the starting city. If no such path exists, returns an empty list.

travelSales :: RoadMap -> Path
travelSales graph =
    let citiesList = cities graph
        n = length citiesList
        cityIndex = zip citiesList [0..]
        getIndex city = snd $ head $ filter (\(c, _) -> c == city) cityIndex
        distMatrix = Data.Array.array ((0, 0), (n - 1, n - 1))
                        [((i, j), case distance graph (citiesList !! i) (citiesList !! j) of
                                        Just dist -> dist
                                        Nothing   -> maxBound)
                         | i <- [0..n-1], j <- [0..n-1]]
        memo = Data.Array.array ((0, 0), (2^n - 1, n - 1))
                    [((mask, i), if mask == (1 `Data.Bits.shiftL` i) then 0 else maxBound) | mask <- [0..(2^n - 1)], i <- [0..(n - 1)]]
        minDist = tspDP (1 `Data.Bits.shiftL` 0) 0 memo distMatrix n
    in if minDist == maxBound then [] else reconstructPath memo distMatrix citiesList 0 (1 `Data.Bits.shiftL` 0)

-- Recursive TSP dynamic programming function with memoization
tspDP :: Int -> Int -> Data.Array.Array (Int, Int) Int -> Data.Array.Array (Int, Int) Int -> Int -> Int
tspDP mask pos memo distMatrix n
    | mask == (2^n - 1) = distMatrix Data.Array.! (pos, 0) -- If all cities visited, return to start
    | memo Data.Array.! (mask, pos) /= maxBound = memo Data.Array.! (mask, pos) -- Return memoized result if available
    | otherwise = let
        nextCities = [next | next <- [0..n-1], not (Data.Bits.testBit mask next), distMatrix Data.Array.! (pos, next) /= maxBound]
        minDist = if null nextCities
                  then maxBound  -- No valid next cities
                  else minimum [distMatrix Data.Array.! (pos, next) + tspDP (mask Data.Bits..|. (1 `Data.Bits.shiftL` next)) next memo distMatrix n | next <- nextCities]

        -- Update memoization table with minimum distance found
        in if minDist == maxBound
           then maxBound  -- If no paths were possible
           else minDist

-- Function to reconstruct the path from the memoization table
reconstructPath :: Data.Array.Array (Int, Int) Int -> Data.Array.Array (Int, Int) Int -> [City] -> Int -> Int -> Path
reconstructPath memo distMatrix cities pos mask
    | mask == (2^length cities - 1) = [cities !! pos, head cities] -- Close the tour
    | otherwise = let
        nextCities = [next | next <- [0..length cities - 1], not (Data.Bits.testBit mask next), distMatrix Data.Array.! (pos, next) /= maxBound]
        nextPath = case nextCities of
            [] -> []  -- No next cities available; return empty path
            _  -> let validNext = [(distMatrix Data.Array.! (pos, next) + memo Data.Array.! (mask Data.Bits..|. (1 `Data.Bits.shiftL` next), next), next) | next <- nextCities]
                     in if null validNext
                        then []
                        else let (nextCost, nextIdx) = minimum validNext  -- Get the minimum cost and corresponding index
                             in if nextCost == maxBound
                                then []  -- If the minimum cost is maxBound, return empty
                                else [nextIdx]  -- Return the next index in a list for further processing

        -- If we have a valid next index, reconstruct the path
        in if null nextPath
           then []  -- No valid next path
           else (cities !! pos) : reconstructPath memo distMatrix cities (head nextPath) (mask Data.Bits..|. (1 `Data.Bits.shiftL` head nextPath))


-- Explanation:
-- `travelSales` is a dynamic programming solution to the Traveling Salesman Problem (TSP), which uses memoization to store partial results and avoid redundant calculations. 
-- The distance between all pairs of cities is precomputed and stored in `distMatrix`, and `memo` keeps track of the minimum distances for subsets of visited cities.
-- The function `tspDP` is a recursive helper that computes the minimum distance for each possible set of visited cities (represented by `mask`) and a current position (`pos`).
-- The `reconstructPath` function uses the memoized data to trace the optimal path taken, building the list of cities visited in the order of the shortest path.

-- Complexity:
-- Time Complexity: O(n^2 * 2^n), where n is the number of cities, due to the subset of cities and each possible city position that must be checked in the recursive calls.
-- Space Complexity: O(n * 2^n), for storing the memoization table with distances and path information for each subset of cities.

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