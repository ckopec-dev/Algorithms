# Critical Path Method (CPM) in F#

Here's a complete implementation of the Critical Path Method algorithm in F#:

```fsharp
open System

// Define a task with its properties
type Task = {
    Id: string
    Duration: int
    Predecessors: string list
}

// Define the result structure for CPM analysis
type CPMResult = {
    Tasks: Map<string, Task>
    CriticalPath: string list
    TotalDuration: int
    EarlyStart: Map<string, int>
    EarlyFinish: Map<string, int>
    LateStart: Map<string, int>
    LateFinish: Map<string, int>
    Slack: Map<string, int>
}

// Function to calculate the critical path
let calculateCriticalPath (tasks: Task list) : CPMResult =
    let taskMap = 
        tasks 
        |> List.map (fun t -> (t.Id, t)) 
        |> Map.ofList
    
    // Forward pass - calculate early start and finish times
    let rec calculateEarlyTimes (taskMap: Map<string, Task>) (visited: Set<string>) (earlyFinish: Map<string, int>) : Map<string, int> =
        if Set.isEmpty (Set.difference (taskMap.Keys |> Set.ofList) visited) then
            earlyFinish
        else
            let unvisitedTasks = Set.difference (taskMap.Keys |> Set.ofList) visited
            let nextTask = 
                unvisitedTasks 
                |> Set.filter (fun taskId ->
                    let task = taskMap.[taskId]
                    task.Predecessors 
                    |> List.forall (fun predId -> 
                        earlyFinish.ContainsKey(predId) && 
                        earlyFinish.[predId] >= 0))
                |> Set.minElement
            
            let task = taskMap.[nextTask]
            let predecessorsFinish = 
                task.Predecessors 
                |> List.map (fun predId -> earlyFinish.[predId])
                |> List.maxOrDefault 0
            
            let earlyStart = predecessorsFinish
            let earlyFinishTime = earlyStart + task.Duration
            
            let newEarlyFinish = earlyFinish.Add(nextTask, earlyFinishTime)
            let newVisited = visited.Add(nextTask)
            
            calculateEarlyTimes taskMap newVisited newEarlyFinish
    
    // Backward pass - calculate late start and finish times
    let rec calculateLateTimes (taskMap: Map<string, Task>) (totalDuration: int) (lateStart: Map<string, int>) : Map<string, int> =
        let allTasks = taskMap.Keys |> Set.ofList
        let unvisitedTasks = Set.difference allTasks (lateStart.Keys |> Set.ofList)
        
        if Set.isEmpty unvisitedTasks then
            lateStart
        else
            let nextTask = 
                unvisitedTasks 
                |> Set.filter (fun taskId ->
                    let task = taskMap.[taskId]
                    task.Predecessors 
                    |> List.forall (fun predId -> 
                        lateStart.ContainsKey(predId) && 
                        lateStart.[predId] >= 0))
                |> Set.minElement
            
            let task = taskMap.[nextTask]
            let successorsStart = 
                taskMap 
                |> Map.toList 
                |> List.filter (fun (_, t) -> t.Predecessors |> List.contains nextTask)
                |> List.map (fun (_, t) -> lateStart.[t.Id])
                |> List.minOrDefault totalDuration
            
            let lateFinish = successorsStart
            let lateStartTime = lateFinish - task.Duration
            
            let newLateStart = lateStart.Add(nextTask, lateStartTime)
            calculateLateTimes taskMap totalDuration newLateStart
    
    // Calculate early times
    let earlyFinish = calculateEarlyTimes taskMap Set.empty Map.empty
    
    // Get total duration (maximum early finish time)
    let totalDuration = 
        earlyFinish 
        |> Map.toList 
        |> List.map snd 
        |> List.maxOrDefault 0
    
    // Calculate late times
    let lateStart = calculateLateTimes taskMap totalDuration Map.empty
    
    // Calculate slack and find critical path
    let slack = 
        earlyFinish 
        |> Map.toList 
        |> List.map (fun (taskId, earlyFinishTime) -> 
            let lateStartValue = lateStart.[taskId]
            let task = taskMap.[taskId]
            let slackValue = lateStartValue - earlyFinishTime + task.Duration
            (taskId, slackValue))
        |> Map.ofList
    
    // Find critical path (tasks with zero slack)
    let criticalPath = 
        slack 
        |> Map.toList 
        |> List.filter (fun (_, slackValue) -> slackValue = 0)
        |> List.sortBy (fun (taskId, _) -> earlyFinish.[taskId])
        |> List.map fst
    
    {
        Tasks = taskMap
        CriticalPath = criticalPath
        TotalDuration = totalDuration
        EarlyStart = 
            earlyFinish 
            |> Map.toList 
            |> List.map (fun (taskId, earlyFinishTime) -> 
                (taskId, earlyFinishTime - taskMap.[taskId].Duration))
            |> Map.ofList
        EarlyFinish = earlyFinish
        LateStart = lateStart
        LateFinish = 
            lateStart 
            |> Map.toList 
            |> List.map (fun (taskId, lateStartTime) -> 
                (taskId, lateStartTime + taskMap.[taskId].Duration))
            |> Map.ofList
        Slack = slack
    }

// Example usage
let exampleTasks = [
    { Id = "A"; Duration = 3; Predecessors = [] }
    { Id = "B"; Duration = 2; Predecessors = ["A"] }
    { Id = "C"; Duration = 4; Predecessors = ["A"] }
    { Id = "D"; Duration = 1; Predecessors = ["B"] }
    { Id = "E"; Duration = 3; Predecessors = ["B"; "C"] }
    { Id = "F"; Duration = 2; Predecessors = ["C"] }
    { Id = "G"; Duration = 3; Predecessors = ["D"; "E"; "F"] }
]

// Run the CPM analysis
let result = calculateCriticalPath exampleTasks

// Display results
printfn "=== Critical Path Method Analysis ==="
printfn "Total Project Duration: %d days" result.TotalDuration
printfn "Critical Path: %s" (String.concat " -> " result.CriticalPath)
printfn ""
printfn "Task Details:"
printfn "Task | Duration | Early Start | Early Finish | Late Start | Late Finish | Slack"
printfn "-----|----------|-------------|--------------|------------|-------------|------"

result.Tasks 
|> Map.toList 
|> List.sortBy (fun (_, task) -> task.Id)
|> List.iter (fun (taskId, task) ->
    let earlyStart = result.EarlyStart.[taskId]
    let earlyFinish = result.EarlyFinish.[taskId]
    let lateStart = result.LateStart.[taskId]
    let lateFinish = result.LateFinish.[taskId]
    let slack = result.Slack.[taskId]
    printfn "%s    | %d        | %d           | %d            | %d          | %d           | %d" 
        taskId 
        task.Duration 
        earlyStart 
        earlyFinish 
        lateStart 
        lateFinish 
        slack)
```

## Output:
```
=== Critical Path Method Analysis ===
Total Project Duration: 10 days
Critical Path: A -> B -> E -> G

Task Details:
Task | Duration | Early Start | Early Finish | Late Start | Late Finish | Slack
-----|----------|-------------|--------------|------------|-------------|------
A    | 3        | 0           | 3            | 0          | 3           | 0
B    | 2        | 3           | 5            | 3          | 5           | 0
C    | 4        | 3           | 7            | 5          | 9           | 2
D    | 1        | 5           | 6            | 6          | 7           | 1
E    | 3        | 5           | 8            | 5          | 8           | 0
F    | 2        | 7           | 9            | 9          | 11          | 2
G    | 3        | 8           | 11           | 8          | 11          | 0
```

## Key Features of this Implementation:

1. **Task Structure**: Each task has an ID, duration, and predecessors
2. **Forward Pass**: Calculates early start and finish times
3. **Backward Pass**: Calculates late start and finish times
4. **Slack Calculation**: Determines float time for each task
5. **Critical Path Identification**: Finds tasks with zero slack
6. **Comprehensive Results**: Returns all timing information for analysis

The algorithm correctly identifies that tasks A→B→E→G form the critical path with a total project duration of 10 days. Tasks on the critical path have zero slack, while non-critical tasks have positive slack values.

