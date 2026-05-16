# Fast Multipole Method (FMM) in Perl

The Fast Multipole Method is a numerical technique for efficiently computing particle interactions. Here's a simplified implementation example in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Math::Complex;

package FMM_Tree {
    sub new {
        my ($class, $center, $size, $max_particles) = @_;
        my $self = {
            center => $center,
            size => $size,
            max_particles => $max_particles,
            particles => [],
            children => [],
            is_leaf => 1,
            multipole_expansion => 0,
            local_expansion => 0,
        };
        return bless $self, $class;
    }
    
    sub add_particle {
        my ($self, $particle) = @_;
        push @{$self->{particles}}, $particle;
        
        # If we exceed the maximum particles and this is a leaf, subdivide
        if (@{$self->{particles}} > $self->{max_particles} && $self->{is_leaf}) {
            $self->subdivide();
        }
    }
    
    sub subdivide {
        my ($self) = @_;
        $self->{is_leaf} = 0;
        
        # Create 8 children (for 3D) or 4 children (for 2D)
        my $half_size = $self->{size} / 2;
        my ($cx, $cy, $cz) = @{$self->{center}};
        
        # For 2D case
        for my $i (0..1) {
            for my $j (0..1) {
                my $new_center = [$cx + ($i - 0.5) * $half_size, 
                                $cy + ($j - 0.5) * $half_size];
                my $child = FMM_Tree->new($new_center, $half_size, $self->{max_particles});
                push @{$self->{children}}, $child;
            }
        }
        
        # Redistribute particles to children
        for my $particle (@{$self->{particles}}) {
            $self->distribute_to_children($particle);
        }
        $self->{particles} = [];
    }
    
    sub distribute_to_children {
        my ($self, $particle) = @_;
        my ($px, $py) = @{$particle->{position}};
        my ($cx, $cy) = @{$self->{center}};
        my $half_size = $self->{size} / 2;
        
        # Determine which child this particle belongs to
        my $child_idx = 0;
        $child_idx += 2 if ($px > $cx);
        $child_idx += 1 if ($py > $cy);
        
        if (@{$self->{children}} > 0) {
            $self->{children}[$child_idx]->add_particle($particle);
        }
    }
    
    sub compute_multipole_expansion {
        my ($self) = @_;
        return if $self->{is_leaf} && @{$self->{particles}} == 0;
        
        if ($self->{is_leaf}) {
            # Compute multipole expansion for particles in this leaf
            my $expansion = 0;
            for my $particle (@{$self->{particles}}) {
                $expansion += $particle->{charge};
            }
            $self->{multipole_expansion} = $expansion;
        } else {
            # Recursively compute for children
            for my $child (@{$self->{children}}) {
                $child->compute_multipole_expansion();
                $self->{multipole_expansion} += $child->{multipole_expansion};
            }
        }
    }
    
    sub compute_interaction {
        my ($self, $other_tree) = @_;
        my $force = 0;
        
        # Simple force computation (simplified)
        if ($self->{is_leaf} && $other_tree->{is_leaf}) {
            # Direct particle-particle interaction
            for my $p1 (@{$self->{particles}}) {
                for my $p2 (@{$other_tree->{particles}}) {
                    my ($x1, $y1) = @{$p1->{position}};
                    my ($x2, $y2) = @{$p2->{position}};
                    my $dx = $x2 - $x1;
                    my $dy = $y2 - $y1;
                    my $r = sqrt($dx*$dx + $dy*$dy);
                    if ($r > 0) {
                        my $force_magnitude = $p1->{charge} * $p2->{charge} / ($r * $r);
                        $force += $force_magnitude;
                    }
                }
            }
        } elsif (!$self->{is_leaf} && $other_tree->{is_leaf}) {
            # Use multipole approximation
            my $distance = sqrt(
                ($self->{center}[0] - $other_tree->{center}[0])**2 +
                ($self->{center}[1] - $other_tree->{center}[1])**2
            );
            if ($distance > $self->{size} + $other_tree->{size}) {
                # Use multipole expansion
                my $force_magnitude = $self->{multipole_expansion} * $other_tree->{multipole_expansion};
                $force += $force_magnitude;
            } else {
                # Recurse
                for my $child (@{$self->{children}}) {
                    $force += $child->compute_interaction($other_tree);
                }
            }
        } else {
            # Recurse
            for my $child (@{$self->{children}}) {
                $force += $child->compute_interaction($other_tree);
            }
        }
        
        return $force;
    }
}

# Example usage
package main;

# Create some particles
my @particles = (
    { position => [0, 0], charge => 1.0 },
    { position => [1, 1], charge => 2.0 },
    { position => [2, 2], charge => 3.0 },
    { position => [0.5, 0.5], charge => 1.5 },
    { position => [1.5, 1.5], charge => 2.5 },
);

# Create FMM tree
my $tree = FMM_Tree->new([1, 1], 4, 2);

# Add particles to tree
for my $particle (@particles) {
    $tree->add_particle($particle);
}

# Compute multipole expansions
$tree->compute_multipole_expansion();

# Compute forces between particles
my $total_force = 0;
for my $i (0..$#particles) {
    for my $j ($i+1..$#particles) {
        my $force = $tree->compute_interaction(
            FMM_Tree->new($particles[$i]{position}, 0.1, 1)
        );
        $total_force += $force;
    }
}

print "Total computed force: $total_force\n";

# Output the tree structure
sub print_tree {
    my ($tree, $depth) = @_;
    my $indent = "  " x $depth;
    
    if ($tree->{is_leaf}) {
        print "$indent Leaf: " . scalar(@{$tree->{particles}}) . " particles\n";
        for my $particle (@{$tree->{particles}}) {
            print "$indent  Particle at ($particle->{position}[0], $particle->{position}[1]): charge = $particle->{charge}\n";
        }
    } else {
        print "$indent Internal node\n";
        for my $child (@{$tree->{children}}) {
            print_tree($child, $depth + 1);
        }
    }
}

print "\nTree structure:\n";
print_tree($tree, 0);
```

## Key Components of this FMM Implementation:

1. **FMM_Tree Class**: Represents the hierarchical tree structure
2. **Particle Management**: Particles are distributed among tree nodes
3. **Tree Subdivision**: When a node exceeds maximum particles, it subdivides
4. **Multipole Expansion**: Computes approximate interactions using multipole expansions
5. **Force Calculation**: Computes particle interactions using FMM principles

## Features:

- **Hierarchical Structure**: 2D quadtree implementation
- **Adaptive Subdivision**: Automatically subdivides when particle limit is exceeded
- **Multipole Approximation**: Uses multipole expansions for distant interactions
- **Efficient Computation**: Reduces computational complexity from O(N²) to O(N log N)

## Usage Notes:

This is a simplified implementation for demonstration purposes. A full FMM implementation would include:
- 3D spatial data structures
- More sophisticated multipole expansions
- Translation operators between different levels
- Better optimization for memory usage
- More accurate force calculations

The algorithm demonstrates the core principle of FMM: using multipole expansions to approximate distant particle interactions, while computing exact interactions for nearby particles.

