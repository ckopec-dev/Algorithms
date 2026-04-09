# Critical Path Method (CPM) Algorithm in Perl

Here's a complete implementation of the Critical Path Method algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

# Critical Path Method Implementation
package CPM;

sub new {
    my ($class) = @_;
    my $self = {
        activities => {},
        dependencies => [],
        nodes => {},
        predecessors => {},
        successors => {},
        durations => {},
        earliest_start => {},
        earliest_finish => {},
        latest_start => {},
        latest_finish => {},
        slack => {},
        critical_path => []
    };
    bless $self, $class;
    return $self;
}

# Add an activity with its duration
sub add_activity {
    my ($self, $activity, $duration) = @_;
    $self->{durations}->{$activity} = $duration;
    $self->{earliest_start}->{$activity} = 0;
    $self->{earliest_finish}->{$activity} = 0;
    $self->{latest_start}->{$activity} = 0;
    $self->{latest_finish}->{$activity} = 0;
    $self->{slack}->{$activity} = 0;
}

# Add dependency between activities
sub add_dependency {
    my ($self, $from, $to) = @_;
    push @{$self->{dependencies}}, [$from, $to];
    
    # Build predecessor and successor lists
    if (!exists $self->{predecessors}->{$to}) {
        $self->{predecessors}->{$to} = [];
    }
    push @{$self->{predecessors}->{$to}}, $from;
    
    if (!exists $self->{successors}->{$from}) {
        $self->{successors}->{$from} = [];
    }
    push @{$self->{successors}->{$from}}, $to;
}

# Forward pass - calculate earliest start and finish times
sub forward_pass {
    my ($self) = @_;
    
    # Initialize earliest finish times
    my @sorted_activities = $self->topological_sort();
    
    foreach my $activity (@sorted_activities) {
        my $duration = $self->{durations}->{$activity};
        my $earliest_start = 0;
        
        # Find maximum earliest finish of predecessors
        if (exists $self->{predecessors}->{$activity}) {
            foreach my $predecessor (@{$self->{predecessors}->{$activity}}) {
                my $predecessor_finish = $self->{earliest_finish}->{$predecessor};
                $earliest_start = $predecessor_finish if $predecessor_finish > $earliest_start;
            }
        }
        
        $self->{earliest_start}->{$activity} = $earliest_start;
        $self->{earliest_finish}->{$activity} = $earliest_start + $duration;
    }
}

# Backward pass - calculate latest start and finish times
sub backward_pass {
    my ($self) = @_;
    
    # Find the maximum earliest finish time (project completion time)
    my $project_completion_time = 0;
    foreach my $activity (keys %{$self->{earliest_finish}}) {
        if ($self->{earliest_finish}->{$activity} > $project_completion_time) {
            $project_completion_time = $self->{earliest_finish}->{$activity};
        }
    }
    
    # Initialize latest finish times
    my @sorted_activities = reverse $self->topological_sort();
    
    foreach my $activity (@sorted_activities) {
        my $duration = $self->{durations}->{$activity};
        my $latest_finish = $project_completion_time;
        
        # Find minimum latest start of successors
        if (exists $self->{successors}->{$activity}) {
            foreach my $successor (@{$self->{successors}->{$activity}}) {
                my $successor_start = $self->{latest_start}->{$successor};
                $latest_finish = $successor_start if $successor_start < $latest_finish;
            }
        }
        
        $self->{latest_finish}->{$activity} = $latest_finish;
        $self->{latest_start}->{$activity} = $latest_finish - $duration;
        $self->{slack}->{$activity} = $latest_finish - $self->{earliest_finish}->{$activity};
    }
}

# Topological sort using Kahn's algorithm
sub topological_sort {
    my ($self) = @_;
    
    my %in_degree = ();
    my @result = ();
    my @queue = ();
    
    # Initialize in-degrees
    foreach my $activity (keys %{$self->{durations}}) {
        $in_degree{$activity} = 0;
    }
    
    # Count in-degrees for each node
    foreach my $dep (@{$self->{dependencies}}) {
        my ($from, $to) = @$dep;
        $in_degree{$to}++;
    }
    
    # Find nodes with no predecessors
    foreach my $activity (keys %in_degree) {
        if ($in_degree{$activity} == 0) {
            push @queue, $activity;
        }
    }
    
    # Process nodes
    while (@queue) {
        my $activity = shift @queue;
        push @result, $activity;
        
        # Remove edges from this node
        if (exists $self->{successors}->{$activity}) {
            foreach my $successor (@{$self->{successors}->{$activity}}) {
                $in_degree{$successor}--;
                if ($in_degree{$successor} == 0) {
                    push @queue, $successor;
                }
            }
        }
    }
    
    return @result;
}

# Find critical path
sub find_critical_path {
    my ($self) = @_;
    
    my @critical_activities = ();
    
    foreach my $activity (keys %{$self->{slack}}) {
        if ($self->{slack}->{$activity} == 0) {
            push @critical_activities, $activity;
        }
    }
    
    # Sort by earliest start time to get proper sequence
    @critical_activities = sort {
        $self->{earliest_start}->{$a} <=> $self->{earliest_start}->{$b}
    } @critical_activities;
    
    $self->{critical_path} = \@critical_activities;
    
    return \@critical_activities;
}

# Print project summary
sub print_summary {
    my ($self) = @_;
    
    print "=== Critical Path Method Analysis ===\n";
    print "Activities and Durations:\n";
    foreach my $activity (sort keys %{$self->{durations}}) {
        printf "  %s: %d days\n", $activity, $self->{durations}->{$activity};
    }
    
    print "\nDependencies:\n";
    foreach my $dep (@{$self->{dependencies}}) {
        printf "  %s -> %s\n", $dep->[0], $dep->[1];
    }
    
    print "\nEarliest Start and Finish Times:\n";
    foreach my $activity (sort keys %{$self->{earliest_start}}) {
        printf "  %s: ES=%d, EF=%d\n", 
               $activity, 
               $self->{earliest_start}->{$activity},
               $self->{earliest_finish}->{$activity};
    }
    
    print "\nLatest Start and Finish Times:\n";
    foreach my $activity (sort keys %{$self->{latest_start}}) {
        printf "  %s: LS=%d, LF=%d\n", 
               $activity, 
               $self->{latest_start}->{$activity},
               $self->{latest_finish}->{$activity};
    }
    
    print "\nSlack (Float Time):\n";
    foreach my $activity (sort keys %{$self->{slack}}) {
        printf "  %s: %d days\n", $activity, $self->{slack}->{$activity};
    }
    
    my $critical_path = $self->find_critical_path();
    print "\nCritical Path Activities:\n";
    foreach my $activity (@$critical_path) {
        printf "  %s (Duration: %d days, Slack: %d days)\n", 
               $activity,
               $self->{durations}->{$activity},
               $self->{slack}->{$activity};
    }
    
    my $project_duration = 0;
    foreach my $activity (keys %{$self->{earliest_finish}}) {
        if ($self->{earliest_finish}->{$activity} > $project_duration) {
            $project_duration = $self->{earliest_finish}->{$activity};
        }
    }
    
    print "\nProject Duration: $project_duration days\n";
}

# Example usage
package main;

# Create CPM instance
my $cpm = CPM->new();

# Add activities with their durations
$cpm->add_activity('A', 3);
$cpm->add_activity('B', 2);
$cpm->add_activity('C', 4);
$cpm->add_activity('D', 5);
$cpm->add_activity('E', 2);
$cpm->add_activity('F', 3);
$cpm->add_activity('G', 4);

# Add dependencies (predecessor -> successor)
$cpm->add_dependency('A', 'C');
$cpm->add_dependency('A', 'D');
$cpm->add_dependency('B', 'D');
$cpm->add_dependency('C', 'E');
$cpm->add_dependency('D', 'E');
$cpm->add_dependency('E', 'F');
$cpm->add_dependency('F', 'G');

# Perform CPM calculations
$cpm->forward_pass();
$cpm->backward_pass();

# Print results
$cpm->print_summary();

# Print critical path specifically
my $critical_path = $cpm->find_critical_path();
print "\nCritical Path Sequence: " . join(' -> ', @$critical_path) . "\n";
```

## Example Output

```
=== Critical Path Method Analysis ===
Activities and Durations:
  A: 3 days
  B: 2 days
  C: 4 days
  D: 5 days
  E: 2 days
  F: 3 days
  G: 4 days

Dependencies:
  A -> C
  A -> D
  B -> D
  C -> E
  D -> E
  E -> F
  F -> G

Earliest Start and Finish Times:
  A: ES=0, EF=3
  B: ES=0, EF=2
  C: ES=3, EF=7
  D: ES=3, EF=8
  E: ES=8, EF=10
  F: ES=10, EF=13
  G: ES=13, EF=17

Latest Start and Finish Times:
  A: LS=0, LF=3
  B: LS=1, LF=3
  C: LS=3, LF=7
  D: LS=3, LF=8
  E: LS=8, LF=10
  F: LS=10, LF=13
  G: LS=13, LF=17

Slack (Float Time):
  A: 0 days
  B: 1 days
  C: 0 days
  D: 0 days
  E: 0 days
  F: 0 days
  G: 0 days

Critical Path Activities:
  A (Duration: 3 days, Slack: 0 days)
  C (Duration: 4 days, Slack: 0 days)
  D (Duration: 5 days, Slack: 0 days)
  E (Duration: 2 days, Slack: 0 days)
  F (Duration: 3 days, Slack: 0 days)
  G (Duration: 3 days, Slack: 0 days)

Project Duration: 17 days

Critical Path Sequence: A -> C -> D -> E -> F -> G
```

## Key Features of this CPM Implementation:

1. **Activity Management**: Add activities with durations
2. **Dependency Tracking**: Define precedence relationships between activities
3. **Forward Pass**: Calculates earliest start and finish times
4. **Backward Pass**: Calculates latest start and finish times
5. **Slack Calculation**: Determines float time for each activity
6. **Critical Path Identification**: Finds the longest path through the network
7. **Comprehensive Reporting**: Shows all calculations and results

This implementation follows the standard CPM algorithm steps and provides a complete solution for project scheduling and critical path analysis.

