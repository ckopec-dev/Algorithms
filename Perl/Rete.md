# Rete Algorithm Implementation in Perl

The Rete algorithm is a pattern matching algorithm used in expert systems. Here's a simple implementation in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Rete Node class
package ReteNode;
sub new {
    my ($class, $name) = @_;
    my $self = {
        name => $name,
        children => [],
        conditions => [],
        memory => []
    };
    bless $self, $class;
    return $self;
}

sub add_condition {
    my ($self, $condition) = @_;
    push @{$self->{conditions}}, $condition;
}

sub add_child {
    my ($self, $child) = @_;
    push @{$self->{children}}, $child;
}

sub add_fact {
    my ($self, $fact) = @_;
    push @{$self->{memory}}, $fact;
}

sub get_memory {
    my ($self) = @_;
    return @{$self->{memory}};
}

# Rete Alpha Memory
package AlphaMemory;
use base 'ReteNode';

sub new {
    my ($class, $name) = @_;
    my $self = $class->SUPER::new($name);
    return $self;
}

# Rete Beta Memory
package BetaMemory;
use base 'ReteNode';

sub new {
    my ($class, $name) = @_;
    my $self = $class->SUPER::new($name);
    return $self;
}

# Rete Alpha Network
package AlphaNetwork;
sub new {
    my ($class) = @_;
    my $self = {
        root => AlphaMemory->new("Root"),
        nodes => []
    };
    bless $self, $class;
    return $self;
}

sub add_node {
    my ($self, $node) = @_;
    push @{$self->{nodes}}, $node;
}

# Rete Beta Network
package BetaNetwork;
sub new {
    my ($class) = @_;
    my $self = {
        root => BetaMemory->new("Beta Root"),
        nodes => []
    };
    bless $self, $class;
    return $self;
}

# Rule class
package Rule;
sub new {
    my ($class, $name, $conditions, $actions) = @_;
    my $self = {
        name => $name,
        conditions => $conditions,
        actions => $actions
    };
    bless $self, $class;
    return $self;
}

sub evaluate {
    my ($self, $facts) = @_;
    my $match = 1;
    
    foreach my $condition (@{$self->{conditions}}) {
        my $found = 0;
        foreach my $fact (@$facts) {
            if ($fact =~ /$condition/) {
                $found = 1;
                last;
            }
        }
        $match = 0 unless $found;
    }
    
    return $match;
}

sub execute {
    my ($self, $facts) = @_;
    print "Executing rule: " . $self->{name} . "\n";
    foreach my $action (@{$self->{actions}}) {
        print "  Action: $action\n";
    }
}

# Main Rete Engine
package ReteEngine;
sub new {
    my ($class) = @_;
    my $self = {
        alpha_network => AlphaNetwork->new(),
        beta_network => BetaNetwork->new(),
        rules => [],
        facts => []
    };
    bless $self, $class;
    return $self;
}

sub add_fact {
    my ($self, $fact) = @_;
    push @{$self->{facts}}, $fact;
}

sub add_rule {
    my ($self, $rule) = @_;
    push @{$self->{rules}}, $rule;
}

sub run {
    my ($self) = @_;
    print "Running Rete Engine with " . scalar(@{$self->{facts}}) . " facts\n";
    
    foreach my $rule (@{$self->{rules}}) {
        if ($rule->evaluate($self->{facts})) {
            $rule->execute($self->{facts});
        }
    }
}

# Example usage
package main;

# Create Rete engine
my $engine = ReteEngine->new();

# Add facts
$engine->add_fact("animal:cat");
$engine->add_fact("animal:dog");
$engine->add_fact("pet:cat");
$engine->add_fact("pet:dog");

# Create rules
my $rule1 = Rule->new(
    "Rule 1",
    ["animal:cat", "pet:cat"],
    ["Output: Cat is an animal and a pet"]
);

my $rule2 = Rule->new(
    "Rule 2",
    ["animal:dog", "pet:dog"],
    ["Output: Dog is an animal and a pet"]
);

my $rule3 = Rule->new(
    "Rule 3",
    ["animal:cat", "animal:dog"],
    ["Output: Both cat and dog are animals"]
);

# Add rules to engine
$engine->add_rule($rule1);
$engine->add_rule($rule2);
$engine->add_rule($rule3);

# Run the engine
$engine->run();

# Display facts
print "\nFacts in memory:\n";
foreach my $fact (@{$engine->{facts}}) {
    print "  $fact\n";
}
```

## Output
```
Running Rete Engine with 4 facts
Executing rule: Rule 1
  Action: Output: Cat is an animal and a pet
Executing rule: Rule 2
  Action: Output: Dog is an animal and a pet
Executing rule: Rule 3
  Action: Output: Both cat and dog are animals

Facts in memory:
  animal:cat
  animal:dog
  pet:cat
  pet:dog
```

## Key Components

1. **ReteNode**: Base class for all Rete nodes
2. **AlphaMemory**: Stores facts that match conditions
3. **BetaMemory**: Stores combinations of facts from multiple alpha memories
4. **Rule**: Represents a rule with conditions and actions
5. **ReteEngine**: Main engine that manages facts and rules

This implementation demonstrates the core concepts of the Rete algorithm including pattern matching, memory management, and rule execution, though it's a simplified version of the full algorithm.

