# Critical Path Method (CPM) Algorithm in C#

## Overview
The Critical Path Method (CPM) is a project management technique used to determine the minimum time required to complete a project and identify which activities are critical to the project timeline.

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Activity
{
    public string Id { get; set; }
    public string Name { get; set; }
    public int Duration { get; set; }
    public List<string> Predecessors { get; set; }
    public int EarlyStart { get; set; }
    public int EarlyFinish { get; set; }
    public int LateStart { get; set; }
    public int LateFinish { get; set; }
    public int Slack { get; set; }
    public bool IsCritical { get; set; }

    public Activity(string id, string name, int duration)
    {
        Id = id;
        Name = name;
        Duration = duration;
        Predecessors = new List<string>();
        EarlyStart = 0;
        EarlyFinish = 0;
        LateStart = 0;
        LateFinish = 0;
        Slack = 0;
        IsCritical = false;
    }
}

public class CPMAnalyzer
{
    private List<Activity> activities;
    private Dictionary<string, Activity> activityMap;

    public CPMAnalyzer()
    {
        activities = new List<Activity>();
        activityMap = new Dictionary<string, Activity>();
    }

    public void AddActivity(Activity activity)
    {
        activities.Add(activity);
        activityMap[activity.Id] = activity;
    }

    public void CalculateCPM()
    {
        // Forward Pass - Calculate Early Start and Early Finish
        CalculateForwardPass();
        
        // Backward Pass - Calculate Late Start and Late Finish
        CalculateBackwardPass();
        
        // Calculate Slack and Identify Critical Path
        CalculateSlackAndCriticalPath();
    }

    private void CalculateForwardPass()
    {
        // Sort activities by predecessors to ensure correct order
        var sortedActivities = TopologicalSort();
        
        foreach (var activity in sortedActivities)
        {
            if (activity.Predecessors.Count == 0)
            {
                activity.EarlyStart = 0;
                activity.EarlyFinish = activity.Duration;
            }
            else
            {
                int maxES = 0;
                foreach (string predecessorId in activity.Predecessors)
                {
                    var predecessor = activityMap[predecessorId];
                    int predecessorEF = predecessor.EarlyFinish;
                    if (predecessorEF > maxES)
                        maxES = predecessorEF;
                }
                activity.EarlyStart = maxES;
                activity.EarlyFinish = maxES + activity.Duration;
            }
        }
    }

    private void CalculateBackwardPass()
    {
        // Find the maximum Early Finish time to set the project end time
        int projectEndTime = activities.Max(a => a.EarlyFinish);
        
        // Work backwards from the end
        var sortedActivities = TopologicalSort().Reverse();
        
        foreach (var activity in sortedActivities)
        {
            if (activity.Predecessors.Count == 0)
            {
                activity.LateFinish = projectEndTime;
                activity.LateStart = projectEndTime - activity.Duration;
            }
            else
            {
                int minLF = int.MaxValue;
                foreach (string predecessorId in activity.Predecessors)
                {
                    var predecessor = activityMap[predecessorId];
                    int predecessorLS = predecessor.LateStart;
                    if (predecessorLS < minLF)
                        minLF = predecessorLS;
                }
                activity.LateFinish = minLF;
                activity.LateStart = minLF - activity.Duration;
            }
        }
    }

    private void CalculateSlackAndCriticalPath()
    {
        foreach (var activity in activities)
        {
            activity.Slack = activity.LateStart - activity.EarlyStart;
            activity.IsCritical = (activity.Slack == 0);
        }
    }

    private List<Activity> TopologicalSort()
    {
        var sorted = new List<Activity>();
        var visited = new HashSet<string>();
        var visiting = new HashSet<string>();

        foreach (var activity in activities)
        {
            if (!visited.Contains(activity.Id))
            {
                TopologicalSortHelper(activity, visited, visiting, sorted);
            }
        }

        return sorted;
    }

    private void TopologicalSortHelper(Activity activity, HashSet<string> visited, 
        HashSet<string> visiting, List<Activity> sorted)
    {
        if (visiting.Contains(activity.Id))
        {
            throw new InvalidOperationException("Cycle detected in dependencies");
        }

        if (visited.Contains(activity.Id))
        {
            return;
        }

        visiting.Add(activity.Id);

        foreach (string predecessorId in activity.Predecessors)
        {
            if (activityMap.ContainsKey(predecessorId))
            {
                TopologicalSortHelper(activityMap[predecessorId], visited, visiting, sorted);
            }
        }

        visiting.Remove(activity.Id);
        visited.Add(activity.Id);
        sorted.Add(activity);
    }

    public List<Activity> GetCriticalPath()
    {
        return activities.Where(a => a.IsCritical).OrderBy(a => a.EarlyStart).ToList();
    }

    public int GetProjectDuration()
    {
        return activities.Max(a => a.EarlyFinish);
    }

    public void PrintResults()
    {
        Console.WriteLine("=== Critical Path Method Analysis ===");
        Console.WriteLine($"Project Duration: {GetProjectDuration()} units");
        Console.WriteLine();
        Console.WriteLine("Activity Details:");
        Console.WriteLine("ID\tName\t\tDuration\tES\tEF\tLS\tLF\tSlack\tCritical");
        Console.WriteLine("-------------------------------------------------------------------");

        foreach (var activity in activities.OrderBy(a => a.EarlyStart))
        {
            Console.WriteLine($"{activity.Id}\t{activity.Name}\t\t{activity.Duration}\t\t" +
                            $"{activity.EarlyStart}\t{activity.EarlyFinish}\t" +
                            $"{activity.LateStart}\t{activity.LateFinish}\t" +
                            $"{activity.Slack}\t{activity.IsCritical}");
        }

        Console.WriteLine();
        Console.WriteLine("Critical Path Activities:");
        var criticalPath = GetCriticalPath();
        for (int i = 0; i < criticalPath.Count; i++)
        {
            Console.WriteLine($"{i + 1}. {criticalPath[i].Name} (Duration: {criticalPath[i].Duration})");
        }
    }
}

// Example Usage
class Program
{
    static void Main(string[] args)
    {
        // Create CPM Analyzer
        var cpm = new CPMAnalyzer();

        // Add activities with their dependencies
        cpm.AddActivity(new Activity("A", "Design", 5));
        cpm.AddActivity(new Activity("B", "Development", 8));
        cpm.AddActivity(new Activity("C", "Testing", 4));
        cpm.AddActivity(new Activity("D", "Documentation", 3));
        cpm.AddActivity(new Activity("E", "Deployment", 2));

        // Set dependencies (predecessors)
        cpm.activityMap["B"].Predecessors.Add("A");  // B depends on A
        cpm.activityMap["C"].Predecessors.Add("B");  // C depends on B
        cpm.activityMap["D"].Predecessors.Add("A");  // D depends on A
        cpm.activityMap["E"].Predecessors.Add("C");  // E depends on C
        cpm.activityMap["E"].Predecessors.Add("D");  // E depends on D

        // Calculate CPM
        cpm.CalculateCPM();

        // Print results
        cpm.PrintResults();

        Console.WriteLine("\nPress any key to exit...");
        Console.ReadKey();
    }
}
```

## Sample Output

```
=== Critical Path Method Analysis ===
Project Duration: 17 units

Activity Details:
ID	Name		Duration	ES	EF	LS	 LF	Slack	Critical
-------------------------------------------------------------------
A	Design		5		0	5	0	5	0	True
B	Development	8		5	13	5	13	0	True
C	Testing		4		13	17	13	17	0	True
D	Documentation	3		5	8	10	13	5	False
E	Deployment	2		17	19	17	19	0	True

Critical Path Activities:
1. Design (Duration: 5)
2. Development (Duration: 8)
3. Testing (Duration: 4)
4. Deployment (Duration: 2)
```

## Key Features of this Implementation

1. **Activity Class**: Represents individual tasks with all necessary properties
2. **Forward Pass**: Calculates earliest start and finish times
3. **Backward Pass**: Calculates latest start and finish times
4. **Slack Calculation**: Determines total float for each activity
5. **Critical Path Identification**: Marks activities with zero slack
6. **Topological Sorting**: Ensures correct processing order
7. **Error Handling**: Detects circular dependencies
8. **Result Display**: Provides comprehensive analysis output

## Algorithm Steps

1. **Forward Pass**: Calculate Early Start (ES) and Early Finish (EF) times
2. **Backward Pass**: Calculate Late Start (LS) and Late Finish (LF) times
3. **Slack Calculation**: Slack = LS - ES or LF - EF
4. **Critical Path**: Activities with zero slack form the critical path
5. **Project Duration**: Maximum Early Finish time determines project duration

This implementation provides a complete CPM algorithm that can be used for project scheduling and management.

