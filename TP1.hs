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



--9
travelSales :: RoadMap -> Path
travelSales = undefined

--10
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]