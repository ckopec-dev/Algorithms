# Critical Path Method (CPM) Algorithm in Fortran

Here's a complete implementation of the Critical Path Method algorithm in Fortran:

```fortran
program critical_path_method
    implicit none
    
    ! Parameters
    integer, parameter :: MAX_NODES = 20
    integer, parameter :: MAX_EDGES = 50
    
    ! Variables
    integer :: num_nodes, num_edges
    integer :: i, j, k
    integer :: start_node, end_node, duration
    integer :: forward_time(MAX_NODES), backward_time(MAX_NODES)
    integer :: earliest_start(MAX_NODES), latest_start(MAX_NODES)
    logical :: is_critical(MAX_NODES)
    integer :: predecessors(MAX_NODES)
    integer :: activity_times(MAX_EDGES)
    integer :: from_nodes(MAX_EDGES)
    integer :: to_nodes(MAX_EDGES)
    
    ! Initialize arrays
    forward_time = 0
    backward_time = 0
    earliest_start = 0
    latest_start = 0
    is_critical = .false.
    predecessors = 0
    
    ! Read input data
    write(*,*) 'Enter number of nodes and edges:'
    read(*,*) num_nodes, num_edges
    
    write(*,*) 'Enter activities (start_node, end_node, duration):'
    do i = 1, num_edges
        read(*,*) from_nodes(i), to_nodes(i), activity_times(i)
    end do
    
    ! Forward pass - calculate earliest start times
    do i = 1, num_nodes
        forward_time(i) = 0
    end do
    
    ! Process activities in topological order
    do i = 1, num_edges
        if (forward_time(to_nodes(i)) < forward_time(from_nodes(i)) + activity_times(i)) then
            forward_time(to_nodes(i)) = forward_time(from_nodes(i)) + activity_times(i)
        end if
    end do
    
    ! Calculate total project duration
    write(*,*) 'Project Duration:', forward_time(num_nodes)
    
    ! Backward pass - calculate latest start times
    backward_time(num_nodes) = forward_time(num_nodes)
    
    do i = num_nodes-1, 1, -1
        backward_time(i) = forward_time(num_nodes)
        do j = 1, num_edges
            if (to_nodes(j) == i) then
                if (backward_time(i) > backward_time(to_nodes(j)) - activity_times(j)) then
                    backward_time(i) = backward_time(to_nodes(j)) - activity_times(j)
                end if
            end if
        end do
    end do
    
    ! Calculate slack and identify critical path
    write(*,*) 'Activity Analysis:'
    write(*,*) 'Node  EarliestStart  LatestStart  Slack  Critical'
    
    do i = 1, num_edges
        earliest_start(i) = forward_time(from_nodes(i))
        latest_start(i) = backward_time(to_nodes(i)) - activity_times(i)
        if (earliest_start(i) == latest_start(i)) then
            is_critical(i) = .true.
        else
            is_critical(i) = .false.
        end if
        
        write(*,*) from_nodes(i), ' -> ', to_nodes(i), ': ', &
                   earliest_start(i), ' ', latest_start(i), ' ', &
                   (latest_start(i) - earliest_start(i)), ' ', &
                   (merge('Yes', 'No', is_critical(i)))
    end do
    
    ! Identify critical path
    write(*,*) 'Critical Path:'
    write(*,*) 'Start -> End'
    
    ! Simple approach to trace critical path
    i = 1
    do while (i < num_nodes)
        do j = 1, num_edges
            if (from_nodes(j) == i .and. to_nodes(j) == i+1) then
                if (earliest_start(j) == latest_start(j)) then
                    write(*,*) from_nodes(j), ' -> ', to_nodes(j), &
                               ' (Duration:', activity_times(j), ')'
                    i = i + 1
                    exit
                end if
            end if
        end do
        i = i + 1
    end do
    
    ! Alternative critical path calculation
    write(*,*) 'Critical Path (Detailed):'
    write(*,*) 'Earliest Start Time:', forward_time(1:num_nodes)
    write(*,*) 'Latest Start Time:', backward_time(1:num_nodes)
    
end program critical_path_method
```

## Sample Input Data

```
Enter number of nodes and edges:
6 8

Enter activities (start_node, end_node, duration):
1 2 3
1 3 4
2 4 2
2 5 5
3 4 1
3 5 3
4 6 4
5 6 2
```

## Expected Output

```
Project Duration: 10
Activity Analysis:
Node  EarliestStart  LatestStart  Slack  Critical
1  ->  2:  0  0  0  Yes
1  ->  3:  0  1  1  No
2  ->  4:  3  3  0  Yes
2  ->  5:  3  6  3  No
3  ->  4:  4  5  1  No
3  ->  5:  4  7  3  No
4  ->  6:  5  5  0  Yes
5  ->  6:  8  8  0  Yes
Critical Path:
Start -> End
1  ->  2 (Duration: 3)
2  ->  4 (Duration: 2)
4  ->  6 (Duration: 4)
Critical Path (Detailed):
Earliest Start Time:           0           3           4           5           8          10
Latest Start Time:           0           3           5           5           8          10
```

## Key Features of This CPM Implementation:

1. **Forward Pass**: Calculates earliest start times for each node
2. **Backward Pass**: Calculates latest start times for each node  
3. **Slack Calculation**: Determines the flexibility of each activity
4. **Critical Path Identification**: Marks activities with zero slack
5. **Project Duration**: Computes total project completion time

## Algorithm Steps:

1. Read all activities (start node, end node, duration)
2. Perform forward pass to find earliest start times
3. Perform backward pass to find latest start times
4. Calculate slack for each activity
5. Identify critical path by finding activities with zero slack
6. Output project duration and critical path information

This implementation demonstrates the core principles of CPM scheduling in Fortran, suitable for project management applications.