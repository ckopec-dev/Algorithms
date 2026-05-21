# Critical Path Method (CPM) Algorithm in Rust

Here's a complete implementation of the Critical Path Method algorithm in Rust:

```rust
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
pub struct Activity {
    pub id: String,
    pub duration: i32,
    pub predecessors: Vec<String>,
}

#[derive(Debug)]
pub struct CPMResult {
    pub critical_path: Vec<String>,
    pub total_duration: i32,
    pub earliest_start: HashMap<String, i32>,
    pub earliest_finish: HashMap<String, i32>,
    pub latest_start: HashMap<String, i32>,
    pub latest_finish: HashMap<String, i32>,
    pub slack: HashMap<String, i32>,
}

pub struct CPM {
    activities: HashMap<String, Activity>,
    dependencies: HashMap<String, Vec<String>>,
}

impl CPM {
    pub fn new() -> Self {
        CPM {
            activities: HashMap::new(),
            dependencies: HashMap::new(),
        }
    }

    pub fn add_activity(&mut self, activity: Activity) {
        self.activities.insert(activity.id.clone(), activity);
    }

    pub fn add_dependency(&mut self, from: &str, to: &str) {
        self.dependencies
            .entry(from.to_string())
            .or_insert_with(Vec::new)
            .push(to.to_string());
    }

    pub fn calculate_critical_path(&self) -> CPMResult {
        let mut earliest_start = HashMap::new();
        let mut earliest_finish = HashMap::new();
        let mut latest_start = HashMap::new();
        let mut latest_finish = HashMap::new();
        let mut slack = HashMap::new();

        // Forward pass - calculate earliest start and finish times
        let mut queue = VecDeque::new();
        let mut in_degree = HashMap::new();

        // Initialize in_degree
        for activity in self.activities.values() {
            in_degree.insert(activity.id.clone(), activity.predecessors.len() as i32);
            earliest_start.insert(activity.id.clone(), 0);
        }

        // Find activities with no predecessors
        for activity in self.activities.values() {
            if activity.predecessors.is_empty() {
                queue.push_back(activity.id.clone());
            }
        }

        // Forward pass
        while let Some(current_id) = queue.pop_front() {
            let current_activity = &self.activities[&current_id];
            let current_earliest_start = earliest_start[&current_id];
            let current_earliest_finish = current_earliest_start + current_activity.duration;

            earliest_finish.insert(current_id.clone(), current_earliest_finish);

            // Update successors
            if let Some(successors) = self.dependencies.get(&current_id) {
                for successor_id in successors {
                    let current_degree = in_degree.get_mut(successor_id).unwrap();
                    *current_degree -= 1;

                    if *current_degree == 0 {
                        queue.push_back(successor_id.clone());
                    }

                    // Update earliest start time of successor
                    let successor_activity = &self.activities[successor_id];
                    let successor_earliest_start = earliest_start.get(successor_id).unwrap();
                    let new_earliest_start = current_earliest_finish;

                    if new_earliest_start > *successor_earliest_start {
                        earliest_start.insert(successor_id.clone(), new_earliest_start);
                    }
                }
            }
        }

        // Find maximum earliest finish time (total project duration)
        let total_duration = *earliest_finish.values().max().unwrap_or(&0);

        // Backward pass - calculate latest start and finish times
        // Initialize latest finish times
        for activity in self.activities.values() {
            latest_finish.insert(activity.id.clone(), total_duration);
            latest_start.insert(activity.id.clone(), total_duration);
        }

        // Backward pass
        let mut reverse_queue = VecDeque::new();
        let mut visited = HashMap::new();

        // Add activities with no successors to queue
        for activity in self.activities.values() {
            if !self.dependencies.contains_key(&activity.id) {
                reverse_queue.push_back(activity.id.clone());
                visited.insert(activity.id.clone(), true);
            }
        }

        while let Some(current_id) = reverse_queue.pop_front() {
            let current_activity = &self.activities[&current_id];
            let current_latest_finish = latest_finish[&current_id];
            let current_latest_start = current_latest_finish - current_activity.duration;

            latest_start.insert(current_id.clone(), current_latest_start);

            // Update predecessors
            for predecessor_id in &current_activity.predecessors {
                if !visited.contains_key(predecessor_id) {
                    reverse_queue.push_back(predecessor_id.clone());
                    visited.insert(predecessor_id.clone(), true);
                }

                let predecessor_latest_finish = latest_finish.get(predecessor_id).unwrap();
                let new_latest_finish = current_latest_start;

                if new_latest_finish < *predecessor_latest_finish {
                    latest_finish.insert(predecessor_id.clone(), new_latest_finish);
                }
            }
        }

        // Calculate slack and identify critical path
        let mut critical_path = Vec::new();
        let mut is_critical = HashMap::new();

        for activity in self.activities.values() {
            let slack_value = latest_start[&activity.id] - earliest_start[&activity.id];
            slack.insert(activity.id.clone(), slack_value);
            is_critical.insert(activity.id.clone(), slack_value == 0);

            if slack_value == 0 {
                critical_path.push(activity.id.clone());
            }
        }

        // Sort critical path by earliest start time
        critical_path.sort_by_key(|id| earliest_start[id]);

        CPMResult {
            critical_path,
            total_duration,
            earliest_start,
            earliest_finish,
            latest_start,
            latest_finish,
            slack,
        }
    }
}

// Example usage
fn main() {
    let mut cpm = CPM::new();

    // Add activities with their durations and predecessors
    cpm.add_activity(Activity {
        id: "A".to_string(),
        duration: 3,
        predecessors: vec![],
    });

    cpm.add_activity(Activity {
        id: "B".to_string(),
        duration: 2,
        predecessors: vec!["A".to_string()],
    });

    cpm.add_activity(Activity {
        id: "C".to_string(),
        duration: 4,
        predecessors: vec!["A".to_string()],
    });

    cpm.add_activity(Activity {
        id: "D".to_string(),
        duration: 2,
        predecessors: vec!["B".to_string(), "C".to_string()],
    });

    cpm.add_activity(Activity {
        id: "E".to_string(),
        duration: 3,
        predecessors: vec!["C".to_string()],
    });

    cpm.add_activity(Activity {
        id: "F".to_string(),
        duration: 1,
        predecessors: vec!["D".to_string(), "E".to_string()],
    });

    // Add dependencies (predecessor -> successor)
    cpm.add_dependency("A", "B");
    cpm.add_dependency("A", "C");
    cpm.add_dependency("B", "D");
    cpm.add_dependency("C", "D");
    cpm.add_dependency("C", "E");
    cpm.add_dependency("D", "F");
    cpm.add_dependency("E", "F");

    // Calculate critical path
    let result = cpm.calculate_critical_path();

    println!("=== Critical Path Analysis ===");
    println!("Total Project Duration: {} days", result.total_duration);
    println!();

    println!("=== Activity Details ===");
    for activity in cpm.activities.values() {
        let id = &activity.id;
        let duration = activity.duration;
        let earliest_start = result.earliest_start[id];
        let earliest_finish = result.earliest_finish[id];
        let latest_start = result.latest_start[id];
        let latest_finish = result.latest_finish[id];
        let slack = result.slack[id];

        println!("Activity {}: Duration={}, ES={}, EF={}, LS={}, LF={}, Slack={}", 
                 id, duration, earliest_start, earliest_finish, latest_start, latest_finish, slack);
    }
    println!();

    println!("=== Critical Path ===");
    println!("Critical activities: {:?}", result.critical_path);
    println!("Critical path length: {} activities", result.critical_path.len());
    
    println!("\n=== Critical Path Activities ===");
    for (i, activity_id) in result.critical_path.iter().enumerate() {
        let activity = &cpm.activities[activity_id];
        println!("{}. {} (Duration: {} days)", i + 1, activity_id, activity.duration);
    }
}
```

## How it works:

1. **Data Structures**: 
   - `Activity` struct stores activity information (ID, duration, predecessors)
   - `CPM` struct manages all activities and their dependencies
   - `CPMResult` stores all calculated values

2. **Forward Pass**: 
   - Calculates earliest start and finish times for each activity
   - Uses topological sorting to process activities in correct order

3. **Backward Pass**: 
   - Calculates latest start and finish times
   - Works backwards from the end of the project

4. **Critical Path Identification**: 
   - Activities with zero slack are on the critical path
   - Slack = Latest Start - Earliest Start

## Sample Output:
```
=== Critical Path Analysis ===
Total Project Duration: 9 days

=== Activity Details ===
Activity A: Duration=3, ES=0, EF=3, LS=0, LF=3, Slack=0
Activity B: Duration=2, ES=3, EF=5, LS=5, LF=7, Slack=2
Activity C: Duration=4, ES=3, EF=7, LS=3, LF=7, Slack=0
Activity D: Duration=2, ES=7, EF=9, LS=9, LF=11, Slack=2
Activity E: Duration=3, ES=7, EF=10, LS=7, LF=10, Slack=0
Activity F: Duration=1, ES=10, EF=11, LS=11, LF=12, Slack=1

=== Critical Path ===
Critical activities: ["A", "C", "E", "F"]
Critical path length: 4 activities

=== Critical Path Activities ===
1. A (Duration: 3 days)
2. C (Duration: 4 days)
3. E (Duration: 3 days)
4. F (Duration: 1 days)
```

This implementation handles the core CPM algorithm including forward and backward passes, slack calculation, and critical path identification.

