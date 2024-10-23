--import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--1
cities :: RoadMap -> [City]
cities = undefined -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this

--2
areAdjacent :: RoadMap -> City -> City -> Bool -- First parameter is the Graph, second and third parameters are the cities names that we will
                                               -- find out if they are adjacent (have a direct path inbetween them), in this case, if there's a tuple
                                               -- with both cities. It returns True if they are adjacent, and False if they are not.

areAdjacent _ c1 c2 | c1 == c2 = False -- Returns False if the cities from the function parameters are the same since they will never be adjacent

areAdjacent [] _ _ = False  -- If the RoadMap is empty, returns False, could happen when the RoadMap starts empty 
                            -- or when no direct path was found inbetween both cities, or one city didn't exist, or both cities are not adjacent

areAdjacent ((rc1, rc2, _):map) c1 c2 -- saves on rc1 and rc2 the cities of the first tuple of the list

        | (((rc1 == c1) && (rc2 == c2)) || ((rc2 == c1) && (rc1 == c2))) = True -- if both cities of the tuple are the same as the cities given in 
                                                                                -- the function parameters, then a direct path was found and returns True

        | otherwise = areAdjacent map c1 c2 -- if this tuple was not a  direct path inbetween the cities of the function parameters, passes the rest of the
                                            -- list and does everything again

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

--5
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

--6
rome :: RoadMap -> [City]
rome [] = []
rome ((c1,c2,_):map) = undefined

--7
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

--8
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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