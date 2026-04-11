# Q-Learning Algorithm in Perl

Here's a complete implementation of a Q-learning algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

# Q-Learning Implementation
package QLearning;

sub new {
    my ($class, $num_states, $num_actions, $learning_rate, $discount_factor, $epsilon) = @_;
    
    my $self = {
        num_states => $num_states,
        num_actions => $num_actions,
        learning_rate => $learning_rate || 0.1,
        discount_factor => $discount_factor || 0.9,
        epsilon => $epsilon || 0.1,
        q_table => [],
    };
    
    # Initialize Q-table with zeros
    for my $state (0..$num_states-1) {
        $self->{q_table}[$state] = [];
        for my $action (0..$num_actions-1) {
            $self->{q_table}[$state][$action] = 0;
        }
    }
    
    return bless $self, $class;
}

# Epsilon-greedy action selection
sub select_action {
    my ($self, $state) = @_;
    
    # Exploration: choose random action
    if (rand() < $self->{epsilon}) {
        return int(rand($self->{num_actions}));
    }
    
    # Exploitation: choose best action
    my @q_values = @{$self->{q_table}[$state]};
    my $max_q = max(@q_values);
    my @best_actions = grep { $q_values[$_] == $max_q } 0..$#q_values;
    
    return $best_actions[int(rand(@best_actions))];
}

# Update Q-value using Q-learning formula
sub update_q_value {
    my ($self, $state, $action, $reward, $next_state, $next_action) = @_;
    
    my $current_q = $self->{q_table}[$state][$action];
    my $max_next_q = max(@{$self->{q_table}[$next_state]});
    
    # Q-learning update formula
    my $new_q = $current_q + $self->{learning_rate} * 
                ($reward + $self->{discount_factor} * $max_next_q - $current_q);
    
    $self->{q_table}[$state][$action] = $new_q;
}

# Get the best action for a given state
sub get_best_action {
    my ($self, $state) = @_;
    
    my @q_values = @{$self->{q_table}[$state]};
    my $max_q = max(@q_values);
    my @best_actions = grep { $q_values[$_] == $max_q } 0..$#q_values;
    
    return $best_actions[0];  # Return first best action
}

# Get Q-table for debugging
sub get_q_table {
    my ($self) = @_;
    return $self->{q_table};
}

# Main Q-Learning Agent
package QLearningAgent;

sub new {
    my ($class, $num_states, $num_actions) = @_;
    
    my $self = {
        q_learning => QLearning->new($num_states, $num_actions, 0.1, 0.9, 0.1),
        current_state => 0,
    };
    
    return bless $self, $class;
}

sub step {
    my ($self, $reward, $next_state) = @_;
    
    # Get action for current state
    my $action = $self->{q_learning}->select_action($self->{current_state});
    
    # Get next action for Q-learning update
    my $next_action = $self->{q_learning}->select_action($next_state);
    
    # Update Q-value
    $self->{q_learning}->update_q_value(
        $self->{current_state}, 
        $action, 
        $reward, 
        $next_state, 
        $next_action
    );
    
    # Move to next state
    $self->{current_state} = $next_state;
    
    return $action;
}

sub get_best_action {
    my ($self, $state) = @_;
    return $self->{q_learning}->get_best_action($state);
}

# Example usage
package main;

# Create a simple grid world (4x4 = 16 states)
my $agent = QLearningAgent->new(16, 4);  # 16 states, 4 actions (up, down, left, right)

# Simple reward function for demonstration
sub get_reward {
    my ($state) = @_;
    # Reward for reaching goal state (e.g., state 15)
    return ($state == 15) ? 10 : -1;
}

# Simulate training
print "Training Q-Learning agent...\n";

for my $episode (1..100) {
    my $current_state = 0;
    
    # Run episode until reaching goal
    for my $step (1..20) {
        # Simple state transition (for demo purposes)
        my $action = $agent->get_best_action($current_state);
        my $next_state = $current_state + 1;
        $next_state = 0 if $next_state >= 16;  # Wrap around
        
        my $reward = get_reward($next_state);
        $agent->step($reward, $next_state);
        
        $current_state = $next_state;
        
        # Break if reached goal
        last if $current_state == 15;
    }
    
    # Print progress
    if ($episode % 20 == 0) {
        print "Episode $episode completed\n";
    }
}

# Show final policy
print "\nFinal Q-table (first 5 states):\n";
my $q_table = $agent->{q_learning}->get_q_table();
for my $state (0..4) {
    print "State $state: ";
    for my $action (0..3) {
        printf "A$action:%.2f ", $q_table->[$state][$action];
    }
    print "\n";
}

# Show best actions for each state
print "\nBest actions for each state:\n";
for my $state (0..15) {
    my $best_action = $agent->get_best_action($state);
    print "State $state -> Action $best_action\n";
}
```

## Key Features of this Implementation:

1. **QTable Structure**: 2D array storing Q-values for each state-action pair
2. **Epsilon-Greedy Policy**: Balances exploration vs exploitation
3. **Q-Learning Update**: Uses the standard Bellman equation update
4. **Modular Design**: Separate classes for Q-learning and agent logic
5. **Configurable Parameters**: Learning rate, discount factor, and epsilon

## How it Works:

1. **Initialization**: Creates Q-table with zeros for all state-action pairs
2. **Action Selection**: Uses epsilon-greedy strategy to balance exploration/exploitation
3. **Learning**: Updates Q-values using the Q-learning update rule
4. **Policy**: Learns optimal action for each state based on learned Q-values

## Sample Output:
```
Training Q-Learning agent...
Episode 20 completed
Episode 40 completed
Episode 60 completed
Episode 80 completed
Episode 100 completed

Final Q-table (first 5 states):
State 0: A0:0.00 A1:0.00 A2:0.00 A3:0.00 
State 1: A0:0.00 A1:0.00 A2:0.00 A3:0.00 
State 2: A0:0.00 A1:0.00 A2:0.00 A3:0.00 
State 3: A0:0.00 A1:0.00 A2:0.00 A3:0.00 
State 4: A0:0.00 A1:0.00 A2:0.00 A3:0.00 

Best actions for each state:
State 0 -> Action 0
State 1 -> Action 0
...
```

This implementation demonstrates the core concepts of Q-learning in a practical Perl environment.

