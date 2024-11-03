module TP1 where

import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]
type AdjList = [(City, [(City, Distance)])]


--Converters -----------------------------------

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
    in adjList2 

convertToRoadMap :: AdjList -> RoadMap
convertToRoadMap adjList = removeDuplicates [(city, neighbor, distance) | (city, neighbors) <- adjList, (neighbor, distance) <- neighbors]

removeDuplicates :: RoadMap -> RoadMap
removeDuplicates [] = []
removeDuplicates ((city1, city2, distance):edges) 
    | (city2, city1, distance) `elem` edges = removeDuplicates edges
    | otherwise = (city1, city2, distance) : removeDuplicates edges

--End of converters--------------------------

--1
cities :: RoadMap -> [City]
cities = undefined -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this

--2
areAdjacent :: RoadMap -> City -> City -> Bool 
areAdjacent map city1 city2 =
        any (\(c1, c2, _)-> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) map

--3
distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined

--4
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((rc1, rc2, dist):map) c1
        | rc1 == c1 = (rc2, dist) : adjacent map c1
        | rc2 == c1 = (rc1, dist) : adjacent map c1
        | otherwise = adjacent map c1 


--adjacent :: RoadMap -> City -> [(City, Distance)]
--adjacent roadmap city = [(if city1 == city then city2 else city1, dist) | (city1, city2, dist) <- roadmap, city1 == city || city2 == city]
--qual seria melhor


--5
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

--6
countAdjacent :: AdjList -> [(City,Int)]
countAdjacent list = map(\(city,adjacent)->(city, length adjacent)) list

rome :: RoadMap -> [City]
rome rmap = 
     let
        list = convertToAdjList rmap
        count = countAdjacent list
        maxC = maximum $ map snd count
     in
        [city | (city, count) <- count, count == maxC]
        
--7
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

--8
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end
    | start == end = [[start]]  -- Return a single path containing just the start city
    | otherwise =
        let adjList = convertToAdjList roadMap
            (minDistance, paths) = findAllPaths adjList start end
        in filter (\path -> calculateDistance adjList path == minDistance) paths

-- Helper function to find all paths
findAllPaths :: AdjList -> City -> City -> (Distance, [Path])
findAllPaths adjList start end = 
    let allPaths = dfs adjList start end [] 0
        minDistance = minimum (map (calculateDistance adjList) allPaths)
    in (minDistance, filter (\path -> calculateDistance adjList path == minDistance) allPaths)

-- Depth-first search to find all paths
dfs :: AdjList -> City -> City -> Path -> Distance -> [Path]
dfs adjList current destination visited distance
    | current == destination = [reverse (current : visited)]
    | current `elem` visited = []
    | otherwise =
        let neighbors = lookupNeighbors current adjList
            newVisited = current : visited
        in concat [dfs adjList neighbor destination newVisited (distance + dist) | (neighbor, dist) <- neighbors]

-- Lookup neighbors of a city
lookupNeighbors :: City -> AdjList -> [(City, Distance)]
lookupNeighbors city adjList = case lookup city adjList of
    Just neighbors -> neighbors
    Nothing -> []

-- Calculate distance of a path
calculateDistance :: AdjList -> Path -> Distance
calculateDistance _ [] = 0
calculateDistance adjList (city1:city2:rest) =
    case lookup city1 adjList of
        Just neighbors ->
            case lookup city2 neighbors of
                Just dist -> dist + calculateDistance adjList (city2:rest)
                Nothing -> maxBound :: Int  -- No connection found
        Nothing -> maxBound :: Int  -- City not found
calculateDistance _ _ = 0  -- If only one city in the path, distance is zero

--9
travelSales :: RoadMap -> Path
travelSales = undefined

--10
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",20),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]