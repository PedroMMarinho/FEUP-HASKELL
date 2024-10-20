import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Debug.Trace (trace) -- retirar no final (bom para debug)
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

-- | PriorityQueue type representing a priorityQueue, where each city is linked with a distance.
type PriorityQueue = [(City,Distance)]

-- | Function: getMinPrioQueue
-- | Goal: Retrieves the city with the minimum distance from the priority queue.
-- | Arguments:
-- |    prioQueue: A PriorityQueue, which is a list of tuples containing a city and its distance.
-- | Returns: A tuple (City, Distance) representing the city with the smallest distance in the priority queue.
-- | Time Complexity: O(n), where n is the number of elements in the priority queue.

getMinPrioQueue :: PriorityQueue -> (City, Distance)
getMinPrioQueue (x:xs) = foldl (\acc@(city1, dist1) (city2, dist2) -> if dist2 < dist1 then (city2, dist2) else acc) x xs

-- | Function: popMinPriorityQueue
-- | Goal: Removes the city with the minimum distance from the priority queue.
-- | Arguments:
-- |    prioQueue: A PriorityQueue, which is a list of tuples containing a city and its distance.
-- |    min: A tuple (City, Distance) representing the city with the minimum distance to be removed.
-- | Returns: A PriorityQueue with the minimum city removed.
-- | Time Complexity: O(n), where n is the number of elements in the priority queue.

popMinPriorityQueue :: PriorityQueue ->  (City, Distance) -> PriorityQueue
popMinPriorityQueue prioQueue min = filter (\(city, _) -> city /= fst min) prioQueue

-- | Function: insertIntoPriorityQueue
-- | Goal: Inserts a city and its distance into the priority queue.
-- | Arguments:
-- |    (city, distance): A tuple where 'city' is the name of the city, and 'distance' is its distance from the source city.
-- |    prioQueue: A PriorityQueue, which is a list of tuples containing cities and their associated distances.
-- | Returns: A new priority queue with the city and its distance added at the front.
-- | Time Complexity: O(1), since the element is added to the front of the list without sorting.

insertIntoPriorityQueue :: (City, Distance) -> PriorityQueue -> PriorityQueue
insertIntoPriorityQueue (city, distance) prioQueue = (city, distance) : prioQueue  -- Add new city with its distance to the priority queue

-- | Function: lookupDistance
-- | Goal: Retrieves the distance of a specific city from a list of distances.
-- | Arguments:
-- |    city: The name of the city whose distance is being looked up.
-- |    distances: A list of tuples where each tuple contains a city and its associated distance.
-- | Returns: The distance of the city if it exists in the list; otherwise, returns error.
-- | Time Complexity: O(n), where n is the number of cities in the distances list.

lookupDistance :: City -> [(City, Distance)] -> Distance
lookupDistance city distances = case lookup city distances of
                    Just distance -> distance
                    Nothing -> error ("City " ++ city ++" is missing from the distanceList")  -- city not found (this case should never happen)


-- | Function: nodesNotVisited
-- | Goal: Retrieves a list of unvisited adjacent nodes from the adjacency list of a given city.
-- | Arguments:
-- |    adjList: The adjacency list representing the graph structure.
-- |    minCity: The city whose neighbors are being checked.
-- |    visited: A list of cities that have already been visited.
-- | Returns: A list of tuples (City, Distance) representing the unvisited neighbors of the city.
-- | Time Complexity: O(k), where k is the number of neighbors of the city .

nodesNotVisited :: AdjList -> City -> [City] -> [(City, Distance)]
nodesNotVisited adjList minCity visited = 
    case lookup minCity adjList of
        Just neighbors -> 
            filter (\(city, _) -> city `notElem` visited) neighbors  -- Keep only unvisited neighbors
        Nothing -> []  -- Return empty list if node has no edges

-- | Function: removeFromPriorityQueue
-- | Goal: Removes a specified city from the priority queue.
-- | Arguments:
-- |    city: The city to be removed from the priority queue.
-- |    prioQueue: A PriorityQueue, which is a list of tuples containing a city and its distance.
-- | Returns: A new priority queue with the specified city removed.
-- | Time Complexity: O(n), where n is the number of elements in the priority queue.

removeFromPriorityQueue :: City -> PriorityQueue -> PriorityQueue
removeFromPriorityQueue city prioQueue = filter (\(c, _) -> c /= city) prioQueue

-- | Function: dijkstra
-- | Goal: Implements Dijkstra's algorithm to find the shortest path from the source to all other cities in the graph.
-- | Arguments:
-- |    adjList: The adjacency list representing the graph structure.
-- |    visited: A list of cities that have already been visited.
-- |    predecessors: A list of tuples (City, [City]) representing the predecessors for each city.
-- |    initialDistances: A list of tuples (City, Distance) representing the current known shortest distance to each city.
-- |    initialPrioQueue: A PriorityQueue of cities ordered by their current known shortest distance.
-- | Returns: A list of tuples (City, [City]) representing the predecessors for each city after completing Dijkstra's algorithm.
-- | Time Complexity: O((V + E) * log V), where V is the number of cities and E is the number of roads.

dijkstra :: AdjList -> [City] -> [(City, [City])] -> [(City, Distance)] -> [(City, Distance)] -> [(City, [City])]
dijkstra adjList visited predecessors initialDistances initialPrioQueue 
    | null initialPrioQueue = predecessors  -- Base case: no more nodes to process
    | otherwise = 
                  dijkstra adjList newVisited newPredecessors newDistances updatedPrioQueue  -- Recursive call
    where 
        min = getMinPrioQueue initialPrioQueue  -- Get the city with the minimum distance
        newPrioQueue = popMinPriorityQueue initialPrioQueue min  -- Remove it from the priority queue
        newVisited = visited ++ [fst min]  -- Add the city to the visited list
        unvisitedNodes = nodesNotVisited adjList (fst min) visited  -- Get unvisited neighbors of the current city
        -- Update distances and predecessors, and prepare to updated priority queue
        (newDistances, newPredecessors, updatedPrioQueue) = foldl updateDistances (initialDistances, predecessors, newPrioQueue) unvisitedNodes
            where
                updateDistances (distances, preds, prioQueue) (neighborCity, weight) =
                    let currentDistance = snd min  -- Distance to the current node
                        newDistance = currentDistance + weight  -- New distance through the current node
                        existingDistance = lookupDistance neighborCity distances  -- Existing distance to the neighbor
                        existingPreds = snd (head $ filter ((== neighborCity) . fst) preds)   -- Get existing predecessors for the neighbor                                  
                    in  
                       case compare newDistance existingDistance of
                        LT ->  -- If the new distance is less
                            let updatedDistances = (neighborCity, newDistance) : filter ((/= neighborCity) . fst) distances
                                updatedPreds = (neighborCity, [fst min]) : filter ((/= neighborCity) . fst) preds  -- Replace the predecessors list
                                updatedPrioQueue = insertIntoPriorityQueue (neighborCity, newDistance) (removeFromPriorityQueue neighborCity prioQueue)
                            in 
                               (updatedDistances, updatedPreds, updatedPrioQueue)  -- Add to priority queue
                        EQ ->  -- If the distances are equal
                                   if fst min `notElem` existingPreds  -- Check if the current city is already a predecessor
                                        then
                                            let updatedPreds = (neighborCity, existingPreds ++ [fst min]) : filter ((/= neighborCity) . fst) preds  -- Append current city to predecessors
                                            in 
                                            (distances, updatedPreds, prioQueue)  -- No update needed for distance
                                        else 
                                            
                                            (distances, preds, prioQueue)  -- No update needed
                        GT ->  -- If the new distance is greater
                            if existingDistance == 100000000  -- Check if the existing distance is infinite
                            then 
                                let updatedDistances = (neighborCity, newDistance) : filter ((/= neighborCity) . fst) distances
                                    updatedPreds = preds ++ [(neighborCity, [fst min])]  -- Add current node to predecessors
                                in 
                                   (updatedDistances, updatedPreds, insertIntoPriorityQueue (neighborCity, newDistance) prioQueue)  -- Add to priority queue
                            else             
                                (distances, preds, prioQueue)  -- No updates needed


-- | Function: lookupPredecessors
-- | Goal: Retrieve the list of predecessor cities for a given target city.
-- | Arguments:
-- |    predecessorsList: A list of tuples (City, [City]) where each city is mapped to its predecessor cities.
-- |    targetCity: The city for which we want to retrieve the predecessors.
-- | Returns: A list of predecessor cities for the given target city.
-- | Time Complexity: O(N), where N is the number of cities in the predecessorsList.

lookupPredecessors :: [(City, [City])] -> City -> [City]
lookupPredecessors [] _ = []
lookupPredecessors ((city, predecessors):rest) targetCity
    | city == targetCity = predecessors
    | otherwise = lookupPredecessors rest targetCity

-- | Function: collectAllPaths
-- | Goal: Recursively find all possible paths from a target city to the source city using the predecessors.
-- | Arguments:
-- |    predecessorsList: A list of tuples (City, [City]) representing predecessors of each city.
-- |    targetCity: The city from which we start the pathfinding (the destination).
-- |    currPath: The current path being built (starting from the target city).
-- | Returns: A list of all possible paths from the target city to the source city.
-- | Time Complexity: O(P), where P is the number of possible paths from the target city to the source.

collectAllPaths :: [(City, [City])] -> City -> Path -> [Path]
collectAllPaths predecessorsList targetCity currPath
    | null previousCities = [currPath]    -- Reached the initial source city (predecessors of that city is an empty list)
    | otherwise = foldl (\pathsAcc predecessor -> collectAllPaths predecessorsList predecessor (predecessor : currPath) ++ pathsAcc ) [] previousCities
    where
        previousCities = lookupPredecessors predecessorsList targetCity
        
-- | Function: shortestPath
-- | Goal: Finds all the shortest paths from the start city to the end city in a roadmap using Dijkstra's algorithm.
-- | Arguments:
-- |    graph: A RoadMap, which is a list of tuples (City, City, Distance), representing connections between cities.
-- |    start: The starting city.
-- |    end: The destination city.
-- | Returns: A list of paths, where each path is a list of cities, representing all possible shortest paths from the start to the end city.
-- | Time Complexity: O((V + E) log V), where V is the number of cities and E is the number of roads. The log V factor comes from the priority queue used in Dijkstra's algorithm.
shortestPath :: RoadMap -> City -> City -> [Path] -- Utilizes dikstra
shortestPath graph start end | start == end = [[start]]
                             | not(end `elem` dfs graph start []) = [[]]
                             | otherwise = collectAllPaths (Debug.Trace.trace ("Auxiliary result from Dijkstra: " ++ show aux) aux) end [end]
                              where adjList = convertToAdjList graph
                                    initialPrio = [(start,0)]
                                    initialDistances = foldl (\acc city -> (if city == start then (city,0) else (city,100000000)) : acc) [] (cities graph)
                                    predecessors = [(start,[])]
                                    visited = []
                                    aux = dijkstra adjList visited predecessors initialDistances initialPrio
                                    
type Bit = Integer

allVisited :: Int -> Bit
allVisited n = (Data.Bits.shiftL 1 n) - 1 -- n being the number of cities

-- Function to check if a city has been visited
isCityVisited :: Bit -> Int -> Bool
isCityVisited mask city = (mask Data.Bits..&. ( Data.Bits.shiftL 1 city)) /= 0

updateMask :: Bit -> Int -> Bit
updateMask mask city = mask Data.Bits..|. (Data.Bits.shiftL 1 city)

-- Function to handle Traveling Salesman Problem
auxTravelSales :: AdjMatrix -> Bit -> Int -> Bit -> Path -> (Distance, Path)
auxTravelSales adjMatrix mask pos visitAll path
    |  mask == visitAll = 
        case adjMatrix Data.Array.! (pos, 0) of 
            Just dist -> (dist, reverse (show pos : path)) -- Return distance and complete path
            Nothing ->  (100000000, []) -- No valid path
    | otherwise = 
        let ((_, _), (maxRow, _)) = Data.Array.bounds adjMatrix
            distances = [ (dist + newDist, newPath)
                        | city <- [0..maxRow], not (isCityVisited mask city),
                          let dist = case adjMatrix Data.Array.! (pos, city) of
                                        Just d -> d
                                        Nothing -> 100000000,  -- Use large value for "infinity"
                          let (newDist, newPath) = auxTravelSales adjMatrix (mask Data.Bits..|. (Data.Bits.shiftL 1 city)) city visitAll (show pos : path)
                        ]
            (minDist, minPath) = minimum distances -- Get minimum distance and corresponding path
        in  (minDist, minPath)
                                                            
travelSales :: RoadMap ->  Path -- fazer a solução das matrizes
travelSales graph | not(isStronglyConnected graph)  = [] -- If the graph is not connected, return an empty path
                  | otherwise = snd (auxTravelSales adjMatrix initialMask 0 visited []) ++ ["0"] -- start at node 0
                    where adjMatrix = convertToAdjMatrix graph 
                          initialMask = Data.Bits.bit 0 -- initial mask is 1 
                          uniqueCities = length (cities graph)
                          visited = allVisited uniqueCities                                                            
                                                                
                          
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
gTest5 = [("1", "2", 1), ("2", "3", 1), ("1", "4", 1), ("4", "3", 1),("1","3",100)]

gTest6 :: RoadMap
gTest6 = [("1", "2", 1), 
         ("1", "3", 1), 
         ("2", "4", 1), 
         ("3", "4", 1), 
         ("2", "5", 2), 
         ("3", "5", 2), 
         ("4", "5", 1)]

gTest7 :: RoadMap
gTest7 = [("0","2",5),("0","3",3),("3","2",1),("3","1",2),("2","1",2),("2","4",2),("1","4",1),("0","4",200)]


gTest8 :: RoadMap
gTest8 = [("0", "1", 6), ("0", "2", 6), ("0", "3", 46), ("0", "4", 31), ("0", "5", 51), ("0", "6", 70), ("0", "7", 18), ("0", "8", 29), ("0", "9", 63), ("0", "10", 18), ("0", "11", 78), ("0", "12", 69), ("0", "13", 43), ("0", "14", 36), ("0", "15", 6), ("0", "16", 51), ("0", "17", 0), ("0", "18", 65), ("0", "19", 86), ("1", "2", 25), ("1", "3", 87), ("1", "4", 99), ("1", "5", 40), ("1", "6", 82), ("1", "7", 61), ("1", "8", 29), ("1", "9", 18), ("1", "10", 91), ("1", "11", 58), ("1", "12", 31), ("1", "13", 91), ("1", "14", 56), ("1", "15", 45), ("1", "16", 42), ("1", "17", 27), ("1", "18", 91), ("1", "19", 6), ("2", "3", 15), ("2", "4", 15), ("2", "5", 43), ("2", "6", 63), ("2", "7", 29), ("2", "8", 18), ("2", "9", 63), ("2", "10", 77), ("2", "11", 35), ("2", "12", 90), ("2", "13", 40), ("2", "14", 71), ("2", "15", 45), ("2", "16", 65), ("2", "17", 82), ("2", "18", 39), ("2", "19", 19), ("3", "4", 69), ("3", "5", 72), ("3", "6", 49), ("3", "7", 89), ("3", "8", 6), ("3", "9", 72), ("3", "10", 45), ("3", "11", 52), ("3", "12", 46), ("3", "13", 7), ("3", "14", 80), ("3", "15", 99), ("3", "16", 98), ("3", "17", 92), ("3", "18", 77), ("3", "19", 2), ("4", "5", 3), ("4", "6", 24), ("4", "7", 57), ("4", "8", 95), ("4", "9", 59), ("4", "10", 41), ("4", "11", 80), ("4", "12", 0), ("4", "13", 5), ("4", "14", 25), ("4", "15", 48), ("4", "16", 13), ("4", "17", 57), ("4", "18", 61), ("4", "19", 99), ("5", "6", 21), ("5", "7", 75), ("5", "8", 33), ("5", "9", 83), ("5", "10", 92), ("5", "11", 59), ("5", "12", 80), ("5", "13", 25), ("5", "14", 8), ("5", "15", 73), ("5", "16", 19), ("5", "17", 9), ("5", "18", 84), ("5", "19", 68), ("6", "7", 91), ("6", "8", 10), ("6", "9", 27), ("6", "10", 92), ("6", "11", 49), ("6", "12", 54), ("6", "13", 84), ("6", "14", 79), ("6", "15", 5), ("6", "16", 14), ("6", "17", 87), ("6", "18", 66), ("6", "19", 53), ("7", "8", 47), ("7", "9", 1), ("7", "10", 78), ("7", "11", 53), ("7", "12", 79), ("7", "13", 94), ("7", "14", 99), ("7", "15", 14), ("7", "16", 47), ("7", "17", 90), ("7", "18", 15), ("7", "19", 46), ("8", "9", 78), ("8", "10", 69), ("8", "11", 42), ("8", "12", 66), ("8", "13", 95), ("8", "14", 10), ("8", "15", 37), ("8", "16", 89), ("8", "17", 73), ("8", "18", 83), ("8", "19", 47), ("9", "10", 20), ("9", "11", 42), ("9", "12", 63), ("9", "13", 19), ("9", "14", 83), ("9", "15", 6), ("9", "16", 62), ("9", "17", 53), ("9", "18", 42), ("9", "19", 52), ("10", "11", 84), ("10", "12", 52), ("10", "13", 30), ("10", "14", 14), ("10", "15", 27), ("10", "16", 58), ("10", "17", 76), ("10", "18", 31), ("10", "19", 5), ("11", "12", 0), ("11", "13", 61), ("11", "14", 49), ("11", "15", 13), ("11", "16", 65), ("11", "17", 24), ("11", "18", 70), ("11", "19", 90), ("12", "13", 70), ("12", "14", 60), ("12", "15", 25), ("12", "16", 81), ("12", "17", 99), ("12", "18", 51), ("12", "19", 29), ("13", "14", 65), ("13", "15", 63), ("13", "16", 28), ("13", "17", 30), ("13", "18", 57), ("13", "19", 23), ("14", "15", 63), ("14", "16", 81), ("14", "17", 4), ("14", "18", 66), ("14", "19", 40), ("15", "16", 48), ("15", "17", 82), ("15", "18", 72), ("15", "19", 28), ("16", "17", 17), ("16", "18", 30), ("16", "19", 51), ("17", "18", 99), ("17", "19", 34), ("18", "19", 29)]

gTest9 :: RoadMap
gTest9 = [("0","1",4),("0","2",1),("2","3",1), ("3","4",1), ("4", "1", 1), ("0","5",2),("5","1",2)]

gTest10 :: RoadMap
gTest10 = [("0","1",8), ("0","2",82), ("0","3",273), ("0","4",87), ("0","5",80), ("0","6",237), ("0","7",75), ("0","8",230), ("0","9",72), ("1","2",175), ("1","3",218), ("1","4",199), ("1","5",156), ("1","6",123), ("1","7",103), ("1","8",278), ("1","9",14), ("2","3",109), ("2","4",294), ("2","5",200), ("2","6",143), ("2","7",157), ("2","8",46), ("2","9",141), ("3","4",211), ("3","5",84), ("3","6",259), ("3","7",109), ("3","8",171), ("3","9",119), ("4","5",138), ("4","6",101), ("4","7",53), ("4","8",112), ("4","9",194), ("5","6",123), ("5","7",132), ("5","8",171), ("5","9",48), ("6","7",130), ("6","8",40), ("6","9",129), ("7","8",191), ("7","9",58), ("8","9",158)]