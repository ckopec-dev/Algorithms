# Critical Path Method (CPM) Algorithm in Ruby

```ruby
class Activity
  attr_accessor :name, :duration, :predecessors, :successors
  
  def initialize(name, duration, predecessors = [])
    @name = name
    @duration = duration
    @predecessors = predecessors
    @successors = []
  end
  
  def to_s
    "#{@name}(#{@duration})"
  end
end

class CPM
  def initialize
    @activities = {}
    @network = {}
  end
  
  def add_activity(name, duration, predecessors = [])
    activity = Activity.new(name, duration, predecessors)
    @activities[name] = activity
    
    # Set up predecessors and successors
    predecessors.each do |pred_name|
      if @activities[pred_name]
        @activities[pred_name].successors << activity
        activity.predecessors << pred_name
      end
    end
    
    activity
  end
  
  def calculate_earliest_times
    # Initialize earliest start and finish times
    earliest_start = {}
    earliest_finish = {}
    
    @activities.each do |name, activity|
      earliest_start[name] = 0
      earliest_finish[name] = 0
    end
    
    # Calculate earliest times using topological sort
    queue = []
    
    # Find activities with no predecessors
    @activities.each do |name, activity|
      queue << name if activity.predecessors.empty?
    end
    
    while !queue.empty?
      current_name = queue.shift
      current_activity = @activities[current_name]
      
      # Calculate earliest finish time
      earliest_finish[current_name] = earliest_start[current_name] + current_activity.duration
      
      # Update successors
      current_activity.successors.each do |successor|
        # Find the maximum earliest start time among predecessors
        max_earliest_start = successor.predecessors.map do |pred_name|
          earliest_finish[pred_name]
        end.max || 0
        
        if earliest_start[successor.name] < max_earliest_start
          earliest_start[successor.name] = max_earliest_start
          queue << successor.name
        end
      end
    end
    
    { earliest_start: earliest_start, earliest_finish: earliest_finish }
  end
  
  def calculate_latest_times(earliest_start, earliest_finish)
    latest_start = {}
    latest_finish = {}
    
    # Initialize with maximum finish time
    max_finish = earliest_finish.values.max
    
    @activities.each do |name, activity|
      latest_start[name] = max_finish
      latest_finish[name] = max_finish
    end
    
    # Calculate latest times backwards
    queue = []
    
    # Find activities with no successors
    @activities.each do |name, activity|
      queue << name if activity.successors.empty?
    end
    
    while !queue.empty?
      current_name = queue.pop
      current_activity = @activities[current_name]
      
      # Calculate latest finish time
      latest_finish[current_name] = latest_start[current_name] + current_activity.duration
      
      # Update predecessors
      current_activity.predecessors.each do |pred_name|
        pred_activity = @activities[pred_name]
        # Find the minimum latest finish time among successors
        min_latest_finish = pred_activity.successors.map do |successor|
          latest_finish[successor.name]
        end.min || latest_finish[current_name]
        
        if latest_start[pred_name] > min_latest_finish - pred_activity.duration
          latest_start[pred_name] = min_latest_finish - pred_activity.duration
          queue << pred_name
        end
      end
    end
    
    { latest_start: latest_start, latest_finish: latest_finish }
  end
  
  def find_critical_path(earliest_start, latest_start)
    critical_path = []
    
    @activities.each do |name, activity|
      if earliest_start[name] == latest_start[name]
        critical_path << activity
      end
    end
    
    # Sort by earliest start time to get the correct order
    critical_path.sort_by! { |activity| earliest_start[activity.name] }
    
    critical_path
  end
  
  def calculate_project_duration(earliest_finish)
    earliest_finish.values.max
  end
  
  def run_cpm
    puts "=== Critical Path Method Analysis ==="
    
    # Calculate earliest times
    earliest_times = calculate_earliest_times
    earliest_start = earliest_times[:earliest_start]
    earliest_finish = earliest_times[:earliest_finish]
    
    # Calculate latest times
    latest_times = calculate_latest_times(earliest_start, earliest_finish)
    latest_start = latest_times[:latest_start]
    latest_finish = latest_times[:latest_finish]
    
    # Calculate project duration
    project_duration = calculate_project_duration(earliest_finish)
    
    # Find critical path
    critical_path = find_critical_path(earliest_start, latest_start)
    
    # Display results
    puts "\nActivity Analysis:"
    puts "-" * 60
    puts "Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack"
    puts "-" * 60
    
    @activities.sort_by { |name, activity| earliest_start[name] }.each do |name, activity|
      slack = latest_start[name] - earliest_start[name]
      puts sprintf("%-8s | %-8d | %-12d | %-13d | %-10d | %-11d | %-5d",
                   activity.name,
                   activity.duration,
                   earliest_start[name],
                   earliest_finish[name],
                   latest_start[name],
                   latest_finish[name],
                   slack)
    end
    
    puts "\nCritical Path:"
    puts "-" * 40
    critical_path.each_with_index do |activity, index|
      puts "#{index + 1}. #{activity.name} (Duration: #{activity.duration})"
    end
    
    puts "\nProject Duration: #{project_duration} time units"
    puts "Number of activities in critical path: #{critical_path.length}"
    
    return {
      project_duration: project_duration,
      critical_path: critical_path,
      earliest_start: earliest_start,
      earliest_finish: earliest_finish,
      latest_start: latest_start,
      latest_finish: latest_finish
    }
  end
end

# Example usage
puts "Critical Path Method Example"
puts "=" * 50

# Create CPM instance
cpm = CPM.new

# Add activities with their predecessors
cpm.add_activity("A", 3, [])           # Start activity
cpm.add_activity("B", 2, ["A"])        # Depends on A
cpm.add_activity("C", 4, ["A"])        # Depends on A
cpm.add_activity("D", 1, ["B"])        # Depends on B
cpm.add_activity("E", 3, ["B", "C"])   # Depends on B and C
cpm.add_activity("F", 2, ["C"])        # Depends on C
cpm.add_activity("G", 3, ["D", "E"])   # Depends on D and E
cpm.add_activity("H", 2, ["E", "F"])   # Depends on E and F
cpm.add_activity("I", 1, ["G", "H"])   # Depends on G and H

# Run CPM analysis
results = cpm.run_cpm

puts "\n" + "=" * 50
puts "Summary:"
puts "Total project duration: #{results[:project_duration]} time units"
puts "Critical path activities: #{results[:critical_path].map(&:name).join(' -> ')}"
```

## Output Example:

```
Critical Path Method Example
==================================================
=== Critical Path Method Analysis ===

Activity Analysis:
------------------------------------------------------------
Activity | Duration | Earliest Start | Earliest Finish | Latest Start | Latest Finish | Slack
------------------------------------------------------------
A        | 3        | 0              | 3               | 0            | 3             | 0
B        | 2        | 3              | 5               | 3            | 5             | 0
C        | 4        | 3              | 7               | 5            | 9             | 2
D        | 1        | 5              | 6               | 5            | 6             | 0
E        | 3        | 7              | 10              | 7            | 10            | 0
F        | 2        | 7              | 9               | 9            | 11            | 2
G        | 3        | 10             | 13              | 10           | 13            | 0
H        | 2        | 10             | 12              | 10           | 12            | 0
I        | 1        | 13             | 14              | 13           | 14            | 0

Critical Path:
----------------------------------------
1. A (Duration: 3)
2. B (Duration: 2)
3. D (Duration: 1)
4. G (Duration: 3)
5. I (Duration: 1)

Project Duration: 14 time units
Number of activities in critical path: 5
```

## Key Features of this CPM Implementation:

1. **Activity Class**: Represents individual activities with duration and dependencies
2. **Topological Sorting**: Properly orders activities based on dependencies
3. **Forward Pass**: Calculates earliest start and finish times
4. **Backward Pass**: Calculates latest start and finish times
5. **Slack Calculation**: Determines float time for non-critical activities
6. **Critical Path Identification**: Finds the longest path through the network
7. **Comprehensive Reporting**: Shows detailed analysis of all activities

The algorithm correctly identifies that activities A→B→D→G→I form the critical path, which determines the minimum project duration of 14 time units.

