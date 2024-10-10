import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import qualified Data.Maybe -- TODO ASK TEACHER IF CAN BE IMPORTED
-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities graph = Data.List.nub (concat [[city1,city2] | (city1,city2,_)<-graph])

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent graph city1 city2 =  not (null [ True |(city1',city2',_)<-graph, (city1' == city1 && city2' == city2) || (city1' == city2 && city2' == city1)] )

distance :: RoadMap -> City -> City -> Maybe Distance
distance graph city1 city2 = Data.Maybe.listToMaybe edges
                            where edges = [ distance |(city1',city2',distance)<-graph, (city1' == city1 && city2' == city2) || (city1' == city2 && city2' == city1)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent graph city = [if city == city1 then (city2,distance) else (city1,distance)  |(city1,city2,distance)<-graph, city == city1 || city == city2 ] 

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance graph path = if all (==True) [areAdjacent graph (path !! index) (path !! (index + 1)) | index<-[0..length path - 2]] then fmap sum $ sequence [ distance graph (path !! index) (path !! (index + 1))| index<-[0..length path - 2]] else Nothing

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]