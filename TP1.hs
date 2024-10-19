import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Debug.Trace (trace)
-- PFL 2024/2025 Practical assignment 1

-- | Type alias for City, representing a city's name as a string.
type City = String

-- | Type alias for Path, representing a list of cities forming a path.
type Path = [City]

-- | Type alias for Distance, representing the distance between two cities as an integer.
type Distance = Int

-- | RoadMap type representing a list of roads, where each road is a tuple of two cities and the distance between them.
type RoadMap = [(City, City, Distance)]

-- | AdjList type representing an adjacency list, where each city is associated with a list of adjacent cities and distances.
type AdjList = [(City, [(City, Distance)])]

-- | AdjMatrix type representing an adjacency matrix using arrays. It stores distances between city indices as `Maybe Distance`.
type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)

-- | Data type for adjacency pointers (likely not used). Each city points to a list of roads and their distances.
data AdjPointers = Place City [(RoadMap, Distance)]


-- | Function: convertToAdjList
-- | Goal: Converts a RoadMap (list of roads between cities) into an adjacency list format.
-- | Arguments:
-- |    graph: A RoadMap representing roads between cities and their distances.
-- | Returns: An AdjList, where each city is associated with a list of adjacent cities and their respective distances.
-- | Time Complexity: O(V * E), where V is the number of cities and E is the number of roads.

convertToAdjList :: RoadMap -> AdjList
convertToAdjList graph = [(vertex,[ adj | adj<-adjacent graph vertex])| vertex<-cities graph]

-- | Goal: Converts a RoadMap (list of roads between cities) into an adjacency matrix format.
-- | Arguments:
-- |    graph: A RoadMap representing roads between cities and their distances.
-- | Returns: An AdjMatrix, where the element at position (i, j) represents the distance between the i-th and j-th cities.
-- | Time Complexity: O(V^2 * E), where V is the number of cities and E is the number of roads. 
-- | The complexity comes from iterating over every pair of cities (O(V^2)) and checking each road in the RoadMap for distance (O(E)).

convertToAdjMatrix :: RoadMap -> AdjMatrix
convertToAdjMatrix graph = Data.Array.array bounds ([((c1,c2), distance graph (show c1) (show c2)) | c1<-[0..length vertices -1],c2<-[0..length vertices -1]])
                        where vertices = Data.List.sort (cities graph)
                              bounds = ((0,0),(length vertices -1,length vertices -1))

-- | Function: cities
-- | Goal: Extracts a list of all unique cities from the RoadMap.
-- | Arguments:
-- |    graph: A RoadMap representing roads between cities and their distances.
-- | Returns: A list of unique cities.
-- | Time Complexity: O(E), where E is the number of roads, since we traverse the list of roads once and then remove duplicates.

cities :: RoadMap -> [City]
cities graph = Data.List.nub (concat [[city1,city2] | (city1,city2,_)<-graph])

-- | Function: areAdjacent
-- | Goal: Checks if two cities are adjacent, i.e., if there is a road between them in the RoadMap.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    city1: The first city.
-- |    city2: The second city.
-- | Returns: True if there is a road between city1 and city2, False otherwise.
-- | Time Complexity: O(E), where E is the number of roads, as we check all roads to see if the cities are adjacent.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent graph city1 city2 =  any (\(city1', city2', _) -> (city1' == city1 && city2' == city2) || (city1' == city2 && city2' == city1)) graph

-- | Function: distance
-- | Goal: Retrieves the distance between two adjacent cities in the RoadMap, if they are adjacent.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    city1: The first city.
-- |    city2: The second city.
-- | Returns: The distance between city1 and city2 wrapped in a `Just`, or Nothing if they are not adjacent.
-- | Time Complexity: O(E), where E is the number of roads, as we traverse the list to find the matching road.

distance :: RoadMap -> City -> City -> Maybe Distance
distance graph city1 city2 | edges == [] = Nothing
                           | otherwise = Just (head edges) 
                            where edges = [ distance |(city1',city2',distance)<-graph, (city1' == city1 && city2' == city2) || (city1' == city2 && city2' == city1)]

-- | Function: adjacent
-- | Goal: Finds all cities adjacent to a given city, along with the distances to them.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    city: The city for which we want to find adjacent cities.
-- | Returns: A list of tuples, where each tuple contains an adjacent city and the distance to it.
-- | Time Complexity: O(E), where E is the number of roads, as we traverse the list to find adjacent cities.

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent graph city = [if city == city1 then (city2,distance) else (city1,distance) |(city1,city2,distance)<-graph, city == city1 || city == city2 ] 

-- | Function: pathDistance
-- | Goal: Calculates the total distance of a path if all cities in the path are adjacent.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    path: A list of cities representing the path.
-- | Returns: The total distance wrapped in a `Just`, or Nothing if any pair of cities in the path are not adjacent.
-- | Time Complexity: O(V * E), where V is the number of cities in the path and E is the number of roads.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance graph path = if any (==False) [areAdjacent graph (path !! index) (path !! (index + 1)) | index<-[0..length path - 2]] then Nothing else fmap sum $ sequence [ distance graph (path !! index) (path !! (index + 1))| index<-[0..length path - 2]]

-- | Function: rome
-- | Goal: Finds the city (or cities) with the highest number of adjacent cities.
-- | Arguments:
-- |    graph: A RoadMap.
-- | Returns: A list of cities with the highest adjacency count.
-- | Time Complexity: O(V * E), where V is the number of cities and E is the number of roads.

rome :: RoadMap -> [City]
rome graph = [city | (city,nAdj)<- city_adj, nAdj == maxAdj] 
            where city_adj = [(city, length (adjacent graph city))| city <-cities graph]
                  maxAdj = maximum (map snd city_adj)

-- | Function: neighbors
-- | Goal: Finds all neighboring cities of a given city.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    node: The city whose neighbors are to be found.
-- | Returns: A list of neighboring cities.
-- | Time Complexity: O(E), where E is the number of roads.

neighbors :: RoadMap -> City -> [City]
neighbors graph node = [city |(city,_)<-(adjacent graph node)]

-- | Function: dfs
-- | Goal: Performs Depth First Search (DFS) from a starting city to traverse the RoadMap.
-- | Arguments:
-- |    graph: A RoadMap.
-- |    node: The starting city for the DFS.
-- |    visited: A list of already visited cities.
-- | Returns: A list of cities visited during the DFS.
-- | Time Complexity: O(V + E), where V is the number of cities and E is the number of roads.

dfs :: RoadMap -> City -> [City] -> [City]
dfs graph node visited | node `elem` visited = visited -- If the city as already been visited return the visited nodes
                       | otherwise = foldl (\acc adj -> dfs graph adj acc) (node : visited) (neighbors graph node) -- here we traverse the adj nodes while storing them recursively in the acc

-- | Function: isStronglyConnected
-- | Goal: Checks if the graph is strongly connected, i.e., if all cities are reachable from a starting city.
-- | Arguments:
-- |    graph: A RoadMap.
-- | Returns: True if the graph is strongly connected, False otherwise.
-- | Time Complexity: O(V + E), where V is the number of cities and E is the number of roads.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected  graph = length (cities graph) == length (dfs graph "0" []) -- Here im assuming "0" is in all graphs (if not can change later )


type PriorityQueue = [(City,Distance)]

getMinPrioQueue :: PriorityQueue -> (City, Distance)
getMinPrioQueue (x:xs) = foldl (\acc@(city1, dist1) (city2, dist2) -> if dist2 < dist1 then (city2, dist2) else acc) x xs

popMinPriorityQueue prioQueue min = filter (\(city, _) -> city /= fst min) prioQueue

-- Function to get unvisited adjacent nodes from the adjacency list
nodesNotVisited :: AdjList -> City -> [City] -> [(City, Distance)]
nodesNotVisited adjList minCity visited = 
    case lookup minCity adjList of
        Just neighbors -> 
            filter (\(city, _) -> city `notElem` visited) neighbors  -- Keep only unvisited neighbors
        Nothing -> []  -- Return empty list if node has no edges


dijkstra :: AdjList -> [City] -> [(City, [City])] -> [(City, Distance)] -> [(City, Distance)] -> [(City, [City])]
dijkstra adjList visited predecessors initialDistances initialPrioQueue 
    | null initialPrioQueue = predecessors  -- Base case: no more nodes to process
    | otherwise = trace ("Processing: " ++ show (fst min) ++ ", Distance: " ++ show (snd min) ++ ", Priority Queue: " ++ show initialPrioQueue) $
                  dijkstra adjList newVisited newPredecessors newDistances updatedPrioQueue  -- Recursive call
    where 
        min = getMinPrioQueue initialPrioQueue  -- Get the city with the minimum distance
        newPrioQueue = popMinPriorityQueue initialPrioQueue min  -- Remove it from the priority queue
        newVisited = visited ++ [fst min]  -- Add the city to the visited list
        unvisitedNodes = nodesNotVisited adjList (fst min) visited  -- Get unvisited neighbors of the current city

        -- Update distances and predecessors, and prepare the updated priority queue
        (newDistances, newPredecessors, updatedPrioQueue) = foldl updateDistances (initialDistances, predecessors, newPrioQueue) unvisitedNodes
            where
                updateDistances (distances, preds, prioQueue) (neighborCity, weight) =
                    let currentDistance = lookupDistance (fst min) distances  -- Distance to the current node
                        newDistance = currentDistance + weight  -- New distance through the current node
                        existingDistance = lookupDistance neighborCity distances  -- Existing distance to the neighbor
                        existingPreds = snd (head $ filter ((== neighborCity) . fst) preds)  -- Get existing predecessors for the neighbor
                    in trace ("Checking neighbor: " ++ neighborCity ++ ", New Distance: " ++ show newDistance ++ ", Existing Distance: " ++ show existingDistance) $
                       case compare newDistance existingDistance of
                        LT ->  -- If the new distance is less
                            let updatedDistances = (neighborCity, newDistance) : filter ((/= neighborCity) . fst) distances
                                updatedPreds = preds ++ [(neighborCity, [fst min])]  -- Add current node to predecessors
                            in trace ("Updated distances: " ++ show updatedDistances ++ ", Updated predecessors: " ++ show updatedPreds) $
                               (updatedDistances, updatedPreds, insertIntoPriorityQueue (neighborCity, newDistance) prioQueue)  -- Add to priority queue
                        EQ ->  -- If the distances are equal
                                   if fst min `notElem` existingPreds  -- Check if the current city is already a predecessor
                                        then
                                            let updatedPreds = (neighborCity, existingPreds ++ [fst min]) : filter ((/= neighborCity) . fst) preds  -- Append current city to predecessors
                                            in trace ("Equal distances for " ++ neighborCity ++ ", Updated predecessors: " ++ show updatedPreds) $
                                            (distances, updatedPreds, prioQueue)  -- No update needed for distance
                                        else 
                                            trace ("No update needed for " ++ neighborCity) $
                                            (distances, preds, prioQueue)  -- No update needed
                        GT ->  -- If the new distance is greater
                            if existingDistance == 100000000  -- Check if the existing distance is infinite
                            then 
                                let updatedDistances = (neighborCity, newDistance) : filter ((/= neighborCity) . fst) distances
                                    updatedPreds = preds ++ [(neighborCity, [fst min])]  -- Add current node to predecessors
                                in trace ("Existing distance was infinite for " ++ neighborCity ++ ", Updated distances: " ++ show updatedDistances ++ ", Updated predecessors: " ++ show updatedPreds) $
                                   (updatedDistances, updatedPreds, insertIntoPriorityQueue (neighborCity, newDistance) prioQueue)  -- Add to priority queue
                            else 
                                trace ("No updates needed for " ++ neighborCity) $
                                (distances, preds, prioQueue)  -- No updates needed

                lookupDistance city distances = case lookup city distances of
                    Just distance -> distance
                    Nothing -> 100000000  -- Return "infinity" if city not found

                -- Function to insert the city with its distance into the priority queue
                insertIntoPriorityQueue (city, distance) prioQueue = (city, distance) : prioQueue  -- Add new city with its distance to the priority queue

getPredecessor :: [(City, [City])] -> City -> [City]
getPredecessor [] _ = []
getPredecessor ((city, predecessors):rest) targetCity
    | city == targetCity = predecessors
    | otherwise = getPredecessor rest targetCity



-- Recursively finds all possible paths from the target city to the source using the city predecessors.
findPathsFromCity :: [(City, [City])] -> City -> Path -> [Path]
findPathsFromCity cityPredecessors targetCity currPath
    | null predecessors = [currPath]    -- Reached the initial source city (predecessors of that city is an empty list)
    | otherwise = foldl (\pathsAcc predecessor -> findPathsFromCity cityPredecessors predecessor (predecessor : currPath) ++ pathsAcc ) [] predecessors
    where
        predecessors = getPredecessor cityPredecessors targetCity
        


shortestPath :: RoadMap -> City -> City -> [Path] -- Utilizar dikstra
shortestPath graph start end | start == end = [[start]]
                             | not(end `elem` dfs graph start []) = [[]]
                             | otherwise = findPathsFromCity (Debug.Trace.trace ("Auxiliary result from Dijkstra: " ++ show aux) aux) end [end]
                              where adjList = convertToAdjList graph
                                    initialPrio = [(start,0)]
                                    initialDistances = foldl (\acc city -> (if city == start then (city,0) else (city,100000000)) : acc) [] (cities graph)
                                    predecessors = [(start,[])]
                                    visited = []
                                    aux = dijkstra adjList visited predecessors initialDistances initialPrio
                                    

travelSales :: RoadMap -> Path -- fazer a solução das matrizes
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

-- More test graphs
gTest4 :: RoadMap
gTest4 =  [("1", "0", 1), ("2", "3", 1), ("5", "6", 1), ("6", "1", 1)]

gTest5 :: RoadMap
gTest5 = [("1", "2", 1), ("2", "3", 1), ("1", "4", 1), ("4", "3", 1)]

gTest6 :: RoadMap
gTest6 = [("1", "2", 1), 
         ("1", "3", 1), 
         ("2", "4", 1), 
         ("3", "4", 1), 
         ("2", "5", 2), 
         ("3", "5", 2), 
         ("4", "5", 1)]

