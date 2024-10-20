# PFL PROJECT GRAPH ALGORITHMS

## PROJECT MEMBERS

- Bruno Miguel Gonçalves Ferreira | up202207863@edu.fe.up.pt
- Pedro Miguel Silva Roleira Marinho | up202206854@edu.fe.up.pt

## PARTICIPATION

- Each one of us did **50%** of the work asked. The two most difficult algorithms were made by each student and the rest of the functions were divided accordingly.

## SHORTEST PATH

- **Algorithm selection**: On the function shortest path we decided to utilize the Dijkstra algorithm rather than any other ones because its time complexity is O((V + E) log V) with **V** being the number of nodes(cities) and **E** the number of edges(roads).

- **Algorithm explanation**: Starting of analysing the function `shortestPath` we have 3 different cases that can happen, the start city is equal to the end city, the end city isnt reachable from the start city (we can use a dfs to see that) and finally if the path is possible. If it is indeed possible we compute Dijkstra Algorithm, but before entering a new function's scope we initialize some important values.

```haskell
adjList = convertToAdjList graph -- use a new data type type AdjList = [(City, [(City, Distance)])] for easier handling
initialPrio = [(start,0)] -- priorityQueue is represented with type PriorityQueue = [(City,Distance)] 
initialDistances = foldl (\acc city -> (if city == start then (city,0) else (city,100000000)) : acc) [] (cities graph) -- set all distances to infinite, minus the start city 
predecessors = [(start,[])] -- keep track of the optimal paths
visited = [] -- keep track of visited nodes
``` 

Going inside `dijkstra` function we can see that, when we empty our priorityQueue the function ends and returns each node precedessors, so we can later on build the paths. Otherwise, we still have to process all of them. Having this in mind, our loop keeps on extracting the city with the minimal distance from the priorityQueue, adds the city to the visited list and iterates trough all unvisited adjacent nodes.

```haskell
min = getMinPrioQueue initialPrioQueue  -- Get the city with the minimum distance (City,Distance)
newPrioQueue = popMinPriorityQueue initialPrioQueue min  -- Remove it from the priority queue
newVisited = visited ++ [fst min]  -- Add the city to the visited list
unvisitedNodes = nodesNotVisited adjList (fst min) visited  -- Get unvisited neighbors of the current city
(newDistances, newPredecessors, updatedPrioQueue) = foldl updateDistances (initialDistances, predecessors, newPrioQueue) unvisitedNodes -- Iterate through the unvisitedNodes applying updateDistances
```

Now we need to look at `updateDistances` and see what it's doing. So before "jumping" to our three different cases `LT`,`EQ` and `GT` we store some important values that we will use later. 

```haskell
currentDistance = snd min  -- Distance to the current node
newDistance = currentDistance + weight  -- New distance through the current node
existingDistance = lookupDistance neighborCity distances  -- Existing distance to the neighbor
existingPreds = snd (head $ filter ((== neighborCity) . fst) preds)   -- Get existing predecessors for the neighbor                                  
```

By comparing the `newDistance` of the city we are processing with the `existingDistance` from the distances list. 
- If the `newDistance < existingDistance` we update the distancesList, store the newBestPredecessorPath to the predecessors list and "decrease the key", replacing from the prioqueue the old (neighboorCity,distance) with (neighboorCity,newDistance) to the priorityQueue.
- If the `newDistance == existingDistance`, meaning there is another path that can have the least cost, we add it to predsList replacing or creating a predecessor, if needed.
- If the  `newDistance > existingDistance` and `existingDistance == INFINITY`,with `INFINITY = 100000000`, we yet again update the distanceList with the neigboorCity and its newDistance, add a new predecessor and insert the new (city,dist) into the priority queue.

After processing `shortestPath gTest5 "1" "3"`  we will get something like this from the dijkstra function: `[("3",["4","2"]),("4",["1"]),("2",["1"]),("1",[])]`

Now we only have to build the path. To do that we use `collectAllPaths` and recursively get all of them.
```
endNode = "3"
startingPath = ["3"]
firstRecursiveCall:  startingPath ++ ["4"]  ------/////------ startingPath ++ ["2"]
secondRecursiveCall: startingPath ++ ["4"] ++ ["1"] ------/////------ startingPath ++ ["2"] ++ ["1"]
thirdRecursiveCall :  startingPath ++ ["4"] ++ ["1"] ++ [] ------/////------ startingPath ++ ["2"] ++ ["1"] ++ [] (stop at empty list)
callBack: We retrieve them in reverse order
```

In the end we will get two paths, in this case with the same size, being `[["1","2","3"],["1","4","3"]]`.

 ## TRAVELSALESMAN


 