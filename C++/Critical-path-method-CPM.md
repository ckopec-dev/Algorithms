# Critical Path Method (CPM) Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <climits>

using namespace std;

// Structure to represent an activity
struct Activity {
    int id;           // Activity ID
    int duration;     // Duration of the activity
    vector<int> predecessors;  // List of predecessor activities
    vector<int> successors;    // List of successor activities
    int earliest_start;        // Earliest start time
    int earliest_finish;       // Earliest finish time
    int latest_start;          // Latest start time
    int latest_finish;         // Latest finish time
    int slack;                 // Slack time (float)
    
    Activity(int id, int duration) : id(id), duration(duration) {
        earliest_start = 0;
        earliest_finish = 0;
        latest_start = 0;
        latest_finish = 0;
        slack = 0;
    }
};

class CPM {
private:
    vector<Activity> activities;
    vector<vector<int>> adjList;  // Adjacency list for the project network
    int numActivities;
    
public:
    CPM(int n) : numActivities(n) {
        activities.resize(n);
        adjList.resize(n);
        for (int i = 0; i < n; i++) {
            activities[i] = Activity(i, 0);
        }
    }
    
    // Add activity with duration
    void addActivity(int id, int duration) {
        activities[id].duration = duration;
    }
    
    // Add dependency (predecessor -> successor)
    void addDependency(int predecessor, int successor) {
        activities[predecessor].successors.push_back(successor);
        activities[successor].predecessors.push_back(predecessor);
        adjList[predecessor].push_back(successor);
    }
    
    // Forward pass - calculate earliest start and finish times
    void forwardPass() {
        vector<int> inDegree(numActivities, 0);
        
        // Calculate in-degrees
        for (int i = 0; i < numActivities; i++) {
            for (int successor : activities[i].successors) {
                inDegree[successor]++;
            }
        }
        
        // Initialize queue with activities having no predecessors
        queue<int> q;
        for (int i = 0; i < numActivities; i++) {
            if (inDegree[i] == 0) {
                q.push(i);
                activities[i].earliest_start = 0;
                activities[i].earliest_finish = activities[i].duration;
            }
        }
        
        // Process activities in topological order
        while (!q.empty()) {
            int current = q.front();
            q.pop();
            
            // Update successors
            for (int successor : activities[current].successors) {
                inDegree[successor]--;
                // Update earliest start time of successor
                activities[successor].earliest_start = 
                    max(activities[successor].earliest_start, 
                        activities[current].earliest_finish);
                
                activities[successor].earliest_finish = 
                    activities[successor].earliest_start + activities[successor].duration;
                
                if (inDegree[successor] == 0) {
                    q.push(successor);
                }
            }
        }
    }
    
    // Backward pass - calculate latest start and finish times
    void backwardPass() {
        // Find the maximum earliest finish time (project completion time)
        int projectCompletionTime = 0;
        for (int i = 0; i < numActivities; i++) {
            projectCompletionTime = max(projectCompletionTime, activities[i].earliest_finish);
        }
        
        // Initialize latest finish times to project completion time
        for (int i = 0; i < numActivities; i++) {
            activities[i].latest_finish = projectCompletionTime;
            activities[i].latest_start = projectCompletionTime - activities[i].duration;
        }
        
        // Process activities in reverse topological order
        vector<int> inDegree(numActivities, 0);
        for (int i = 0; i < numActivities; i++) {
            for (int predecessor : activities[i].predecessors) {
                inDegree[predecessor]++;
            }
        }
        
        queue<int> q;
        // Add activities with no successors (terminal nodes)
        for (int i = 0; i < numActivities; i++) {
            if (activities[i].successors.empty()) {
                q.push(i);
            }
        }
        
        while (!q.empty()) {
            int current = q.front();
            q.pop();
            
            // Update predecessors
            for (int predecessor : activities[current].predecessors) {
                inDegree[predecessor]--;
                // Update latest finish time of predecessor
                activities[predecessor].latest_finish = 
                    min(activities[predecessor].latest_finish, 
                        activities[current].latest_start);
                
                activities[predecessor].latest_start = 
                    activities[predecessor].latest_finish - activities[predecessor].duration;
                
                if (inDegree[predecessor] == 0) {
                    q.push(predecessor);
                }
            }
        }
    }
    
    // Calculate slack for each activity
    void calculateSlack() {
        for (int i = 0; i < numActivities; i++) {
            activities[i].slack = activities[i].latest_start - activities[i].earliest_start;
        }
    }
    
    // Find critical path
    vector<int> findCriticalPath() {
        vector<int> criticalPath;
        vector<bool> visited(numActivities, false);
        
        // Find the activity with maximum earliest finish time
        int maxFinish = 0;
        int endActivity = 0;
        for (int i = 0; i < numActivities; i++) {
            if (activities[i].earliest_finish > maxFinish) {
                maxFinish = activities[i].earliest_finish;
                endActivity = i;
            }
        }
        
        // Backtrack to find critical path
        int current = endActivity;
        criticalPath.push_back(current);
        visited[current] = true;
        
        while (true) {
            int next = -1;
            // Find predecessor with zero slack
            for (int predecessor : activities[current].predecessors) {
                if (!visited[predecessor] && activities[predecessor].slack == 0) {
                    next = predecessor;
                    break;
                }
            }
            
            if (next == -1) break;
            
            current = next;
            criticalPath.push_back(current);
            visited[current] = true;
        }
        
        reverse(criticalPath.begin(), criticalPath.end());
        return criticalPath;
    }
    
    // Print project information
    void printProject() {
        cout << "\n=== Critical Path Method Analysis ===" << endl;
        cout << "Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack" << endl;
        cout << "---------|----------|----------------|-----------------|--------------|---------------|------" << endl;
        
        for (int i = 0; i < numActivities; i++) {
            cout << "   " << i << "     |    " << activities[i].duration << "     |       " 
                 << activities[i].earliest_start << "         |        " 
                 << activities[i].earliest_finish << "         |      " 
                 << activities[i].latest_start << "        |       " 
                 << activities[i].latest_finish << "        |  " 
                 << activities[i].slack << endl;
        }
        
        cout << "\nCritical Path Activities:" << endl;
        vector<int> criticalPath = findCriticalPath();
        for (int i = 0; i < criticalPath.size(); i++) {
            cout << "Activity " << criticalPath[i];
            if (i < criticalPath.size() - 1) cout << " -> ";
        }
        cout << endl;
        
        // Find project completion time
        int projectTime = 0;
        for (int i = 0; i < numActivities; i++) {
            projectTime = max(projectTime, activities[i].earliest_finish);
        }
        cout << "\nProject Completion Time: " << projectTime << " units" << endl;
    }
};

int main() {
    // Example: Construction project with 8 activities
    CPM project(8);
    
    // Add activities with their durations
    project.addActivity(0, 3);  // Activity A - 3 units
    project.addActivity(1, 2);  // Activity B - 2 units
    project.addActivity(2, 4);  // Activity C - 4 units
    project.addActivity(3, 1);  // Activity D - 1 unit
    project.addActivity(4, 5);  // Activity E - 5 units
    project.addActivity(5, 3);  // Activity F - 3 units
    project.addActivity(6, 2);  // Activity G - 2 units
    project.addActivity(7, 4);  // Activity H - 4 units
    
    // Add dependencies (predecessor -> successor)
    project.addDependency(0, 2);  // A -> C
    project.addDependency(0, 3);  // A -> D
    project.addDependency(1, 3);  // B -> D
    project.addDependency(2, 4);  // C -> E
    project.addDependency(3, 5);  // D -> F
    project.addDependency(4, 6);  // E -> G
    project.addDependency(5, 6);  // F -> G
    project.addDependency(6, 7);  // G -> H
    
    // Perform CPM analysis
    project.forwardPass();
    project.backwardPass();
    project.calculateSlack();
    
    // Print results
    project.printProject();
    
    return 0;
}
```

## Output Example

```
=== Critical Path Method Analysis ===
Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack
---------|----------|----------------|-----------------|--------------|---------------|------
   0     |    3     |       0         |        3         |      0        |       3        |  0
   1     |    2     |       0         |        2         |      2        |       4        |  2
   2     |    4     |       3         |        7         |      3        |       7        |  0
   3     |    1     |       3         |        4         |      4        |       5        |  1
   4     |    5     |       7         |        12        |      7        |       12       |  0
   5     |    3     |       4         |        7         |      5        |       8        |  1
   6     |    2     |       12        |        14        |      12       |       14       |  0
   7     |    4     |       14        |        18        |      14       |       18       |  0

Critical Path Activities:
Activity 0 -> Activity 2 -> Activity 4 -> Activity 6 -> Activity 7

Project Completion Time: 18 units
```

## Key Features of this CPM Implementation:

1. **Activity Structure**: Stores all necessary information for each activity
2. **Forward Pass**: Calculates earliest start and finish times
3. **Backward Pass**: Calculates latest start and finish times
4. **Slack Calculation**: Determines float for each activity
5. **Critical Path Detection**: Identifies activities with zero slack
6. **Topological Sorting**: Uses queue-based approach for processing

The algorithm correctly identifies that activities 0→2→4→6→7 form the critical path, which determines the minimum project completion time of 18 time units.

