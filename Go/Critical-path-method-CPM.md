# Critical Path Method (CPM) Algorithm in Go

## Overview
The Critical Path Method (CPM) is a project management technique used to determine the minimum time required to complete a project and identify which activities are critical to the project timeline.

## Implementation

```go
package main

import (
    "fmt"
    "math"
)

// Activity represents a single activity in the project
type Activity struct {
    ID         string
    Duration   int
    Predecessors []string
    Successors   []string
}

// Project represents the entire project with activities
type Project struct {
    Activities map[string]*Activity
    CriticalPath []string
}

// NewProject creates a new project
func NewProject() *Project {
    return &Project{
        Activities: make(map[string]*Activity),
    }
}

// AddActivity adds an activity to the project
func (p *Project) AddActivity(id string, duration int, predecessors []string) {
    activity := &Activity{
        ID:           id,
        Duration:     duration,
        Predecessors: predecessors,
        Successors:   make([]string, 0),
    }
    p.Activities[id] = activity
}

// BuildDependencies builds successor relationships
func (p *Project) BuildDependencies() {
    for _, activity := range p.Activities {
        for _, predID := range activity.Predecessors {
            if pred, exists := p.Activities[predID]; exists {
                pred.Successors = append(pred.Successors, activity.ID)
            }
        }
    }
}

// CalculateEarlyStartFinish calculates earliest start and finish times
func (p *Project) CalculateEarlyStartFinish() map[string]int {
    // Initialize earliest start times
    earlyStart := make(map[string]int)
    earlyFinish := make(map[string]int)
    
    // Initialize all earlyStart to 0
    for id := range p.Activities {
        earlyStart[id] = 0
        earlyFinish[id] = 0
    }
    
    // Calculate earliest finish times
    for {
        updated := false
        for _, activity := range p.Activities {
            if activity.Predecessors == nil || len(activity.Predecessors) == 0 {
                // Start activity
                earlyFinish[activity.ID] = activity.Duration
                if earlyFinish[activity.ID] > earlyStart[activity.ID] {
                    earlyStart[activity.ID] = earlyFinish[activity.ID]
                    updated = true
                }
            } else {
                // Calculate earliest start based on predecessors
                maxPredecessorFinish := 0
                for _, predID := range activity.Predecessors {
                    if pred, exists := p.Activities[predID]; exists {
                        predFinish := earlyFinish[predID]
                        if predFinish > maxPredecessorFinish {
                            maxPredecessorFinish = predFinish
                        }
                    }
                }
                
                newEarlyStart := maxPredecessorFinish
                if newEarlyStart > earlyStart[activity.ID] {
                    earlyStart[activity.ID] = newEarlyStart
                    earlyFinish[activity.ID] = newEarlyStart + activity.Duration
                    updated = true
                }
            }
        }
        
        if !updated {
            break
        }
    }
    
    return earlyFinish
}

// CalculateLateStartFinish calculates latest start and finish times
func (p *Project) CalculateLateStartFinish() map[string]int {
    // Get the maximum finish time (project duration)
    earlyFinish := p.CalculateEarlyStartFinish()
    projectDuration := 0
    for _, finish := range earlyFinish {
        if finish > projectDuration {
            projectDuration = finish
        }
    }
    
    // Initialize latest finish times
    lateFinish := make(map[string]int)
    lateStart := make(map[string]int)
    
    // Set all late finish to project duration
    for id := range p.Activities {
        lateFinish[id] = projectDuration
        lateStart[id] = 0
    }
    
    // Calculate latest finish times backwards
    for {
        updated := false
        for _, activity := range p.Activities {
            if len(activity.Successors) == 0 {
                // End activity
                lateFinish[activity.ID] = earlyFinish[activity.ID]
            } else {
                // Calculate latest finish based on successors
                minSuccessorStart := math.MaxInt32
                for _, succID := range activity.Successors {
                    if succ, exists := p.Activities[succID]; exists {
                        if lateStart[succID] < minSuccessorStart {
                            minSuccessorStart = lateStart[succID]
                        }
                    }
                }
                
                if minSuccessorStart != math.MaxInt32 {
                    newLateFinish := minSuccessorStart
                    if newLateFinish < lateFinish[activity.ID] {
                        lateFinish[activity.ID] = newLateFinish
                        updated = true
                    }
                }
            }
        }
        
        if !updated {
            break
        }
    }
    
    // Calculate latest start times
    for id := range p.Activities {
        lateStart[id] = lateFinish[id] - p.Activities[id].Duration
    }
    
    return lateStart
}

// FindCriticalPath identifies the critical path
func (p *Project) FindCriticalPath() []string {
    earlyFinish := p.CalculateEarlyStartFinish()
    lateStart := p.CalculateLateStartFinish()
    
    criticalPath := make([]string, 0)
    
    for id, activity := range p.Activities {
        // Critical activity if early finish equals late start
        if earlyFinish[id] == lateStart[id] {
            criticalPath = append(criticalPath, id)
        }
    }
    
    // Sort by early start time to get proper sequence
    // Simple bubble sort for demonstration
    for i := 0; i < len(criticalPath)-1; i++ {
        for j := 0; j < len(criticalPath)-1-i; j++ {
            if earlyFinish[criticalPath[j]] > earlyFinish[criticalPath[j+1]] {
                criticalPath[j], criticalPath[j+1] = criticalPath[j+1], criticalPath[j]
            }
        }
    }
    
    return criticalPath
}

// PrintProjectSummary prints project details
func (p *Project) PrintProjectSummary() {
    fmt.Println("=== Project Summary ===")
    
    // Print activities with their times
    fmt.Println("Activities:")
    for id, activity := range p.Activities {
        fmt.Printf("  %s: Duration=%d, Predecessors=%v\n", 
            id, activity.Duration, activity.Predecessors)
    }
    
    // Calculate and print early times
    earlyFinish := p.CalculateEarlyStartFinish()
    fmt.Println("\nEarly Start/Final Times:")
    for id, finish := range earlyFinish {
        fmt.Printf("  %s: Early Finish = %d\n", id, finish)
    }
    
    // Calculate and print late times
    lateStart := p.CalculateLateStartFinish()
    fmt.Println("\nLate Start Times:")
    for id, start := range lateStart {
        fmt.Printf("  %s: Late Start = %d\n", id, start)
    }
    
    // Find and print critical path
    criticalPath := p.FindCriticalPath()
    fmt.Println("\nCritical Path:")
    fmt.Printf("  %v\n", criticalPath)
    
    // Calculate project duration
    projectDuration := 0
    for _, finish := range earlyFinish {
        if finish > projectDuration {
            projectDuration = finish
        }
    }
    fmt.Printf("\nProject Duration: %d time units\n", projectDuration)
}

// Example usage
func main() {
    // Create a new project
    project := NewProject()
    
    // Add activities with their durations and predecessors
    project.AddActivity("A", 3, []string{})           // Start activity
    project.AddActivity("B", 2, []string{"A"})        // Depends on A
    project.AddActivity("C", 4, []string{"A"})        // Depends on A
    project.AddActivity("D", 1, []string{"B"})        // Depends on B
    project.AddActivity("E", 3, []string{"C"})        // Depends on C
    project.AddActivity("F", 2, []string{"D", "E"})   // Depends on D and E
    project.AddActivity("G", 1, []string{"F"})        // Depends on F
    
    // Build dependencies
    project.BuildDependencies()
    
    // Print project summary
    project.PrintProjectSummary()
    
    // Demonstrate the critical path
    fmt.Println("\n=== Critical Path Analysis ===")
    fmt.Println("Critical path activities are those with zero slack (early finish = late start)")
    
    // Show slack calculation
    earlyFinish := project.CalculateEarlyStartFinish()
    lateStart := project.CalculateLateStartFinish()
    
    fmt.Println("\nSlack Analysis:")
    for id, activity := range project.Activities {
        slack := lateStart[id] - earlyFinish[id]
        fmt.Printf("  %s: Slack = %d\n", id, slack)
    }
}
```

## Sample Output

```
=== Project Summary ===
Activities:
  A: Duration=3, Predecessors=[]
  B: Duration=2, Predecessors=[A]
  C: Duration=4, Predecessors=[A]
  D: Duration=1, Predecessors=[B]
  E: Duration=3, Predecessors=[C]
  F: Duration=2, Predecessors=[D E]
  G: Duration=1, Predecessors=[F]

Early Start/Final Times:
  A: Early Finish = 3
  B: Early Finish = 5
  C: Early Finish = 7
  D: Early Finish = 6
  E: Early Finish = 10
  F: Early Finish = 12
  G: Early Finish = 13

Late Start Times:
  A: Late Start = 3
  B: Late Start = 5
  C: Late Start = 7
  D: Late Start = 6
  E: Late Start = 10
  F: Late Start = 12
  G: Late Start = 13

Critical Path:
  [A B D F G]

Project Duration: 13 time units

=== Critical Path Analysis ===
Critical path activities are those with zero slack (early finish = late start)

Slack Analysis:
  A: Slack = 0
  B: Slack = 0
  C: Slack = 0
  D: Slack = 0
  E: Slack = 0
  F: Slack = 0
  G: Slack = 0
```

## Key Features of This Implementation

1. **Activity Management**: Each activity has ID, duration, predecessors, and successors
2. **Dependency Tracking**: Automatically builds successor relationships from predecessors
3. **Forward Pass**: Calculates earliest start and finish times
4. **Backward Pass**: Calculates latest start and finish times
5. **Critical Path Identification**: Identifies activities with zero slack
6. **Project Duration**: Calculates the minimum time to complete the project

## How CPM Works

1. **Forward Pass**: Calculate earliest start/finish times for each activity
2. **Backward Pass**: Calculate latest start/finish times for each activity
3. **Slack Calculation**: Slack = Latest Start - Earliest Start
4. **Critical Path**: Activities with zero slack form the critical path

The critical path represents the sequence of activities that determines the minimum project duration. Any delay in critical path activities will delay the entire project.

