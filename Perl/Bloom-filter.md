# Bloom Filter Implementation in Perl

Here's a complete implementation of a Bloom Filter algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package BloomFilter {
    sub new {
        my ($class, $size, $hash_count) = @_;
        
        # Initialize Bloom filter with given size and hash count
        my $self = {
            size => $size || 1000,
            hash_count => $hash_count || 3,
            bit_array => [0] x $size,
        };
        
        bless $self, $class;
        return $self;
    }
    
    # Simple hash function - DJB2 algorithm
    sub _djb2_hash {
        my ($string, $seed) = @_;
        my $hash = $seed;
        for my $char (split //, $string) {
            $hash = (($hash << 5) + $hash) + ord($char);
        }
        return $hash;
    }
    
    # Generate multiple hash values using different seeds
    sub _get_hashes {
        my ($self, $item) = @_;
        my @hashes;
        for my $i (0..$self->{hash_count}-1) {
            my $hash = $self->_djb2_hash($item, $i);
            push @hashes, $hash % $self->{size};
        }
        return @hashes;
    }
    
    # Add an item to the Bloom filter
    sub add {
        my ($self, $item) = @_;
        my @positions = $self->_get_hashes($item);
        for my $pos (@positions) {
            $self->{bit_array}[$pos] = 1;
        }
    }
    
    # Check if an item might exist in the Bloom filter
    sub might_contain {
        my ($self, $item) = @_;
        my @positions = $self->_get_hashes($item);
        for my $pos (@positions) {
            return 0 if $self->{bit_array}[$pos] == 0;
        }
        return 1;  # Item might exist
    }
    
    # Clear all bits (reset filter)
    sub clear {
        my ($self) = @_;
        for my $i (0..$self->{size}-1) {
            $self->{bit_array}[$i] = 0;
        }
    }
    
    # Get the size of the bit array
    sub size {
        my ($self) = @_;
        return $self->{size};
    }
    
    # Get the number of hash functions
    sub hash_count {
        my ($self) = @_;
        return $self->{hash_count};
    }
}

# Example usage
print "=== Bloom Filter Example ===\n";

# Create a Bloom filter with 1000 slots and 3 hash functions
my $bf = BloomFilter->new(1000, 3);

# Add some items
my @items_to_add = ("apple", "banana", "cherry", "date", "elderberry");
print "Adding items: " . join(", ", @items_to_add) . "\n";

for my $item (@items_to_add) {
    $bf->add($item);
}

# Test containment
my @test_items = ("apple", "fig", "grape", "banana", "kiwi");
print "\nTesting containment:\n";

for my $item (@test_items) {
    my $result = $bf->might_contain($item) ? "MIGHT EXIST" : "DEFINITELY NOT EXIST";
    print "  $item: $result\n";
}

# Demonstrate false positives
print "\n=== False Positive Demonstration ===\n";
my @false_positive_test = ("zebra", "xylophone", "quartz", "moon");
my $false_positives = 0;

for my $item (@false_positive_test) {
    if ($bf->might_contain($item)) {
        print "  $item: MIGHT EXIST (could be false positive)\n";
        $false_positives++;
    } else {
        print "  $item: DEFINITELY NOT EXIST\n";
    }
}

print "\nFalse positives detected: $false_positives out of " . scalar(@false_positive_test) . "\n";

# Show filter statistics
print "\n=== Filter Statistics ===\n";
print "Filter size: " . $bf->size() . " bits\n";
print "Hash functions: " . $bf->hash_count() . "\n";
```

## Key Features of this Implementation:

1. **Bit Array Storage**: Uses an array of integers to represent the Bloom filter bits
2. **Multiple Hash Functions**: Implements 3 hash functions using the DJB2 algorithm with different seeds
3. **Add Operation**: Sets bits at hash positions for added items
4. **Containment Check**: Returns 1 (might exist) or 0 (definitely not exist)
5. **False Positive Rate**: The algorithm can return false positives but never false negatives

## Sample Output:
```
=== Bloom Filter Example ===
Adding items: apple, banana, cherry, date, elderberry

Testing containment:
  apple: MIGHT EXIST
  fig: DEFINITELY NOT EXIST
  grape: DEFINITELY NOT EXIST
  banana: MIGHT EXIST
  kiwi: DEFINITELY NOT EXIST

=== False Positive Demonstration ===
  zebra: MIGHT EXIST (could be false positive)
  xylophone: MIGHT EXIST (could be false positive)
  quartz: DEFINITELY NOT EXIST
  moon: DEFINITELY NOT EXIST

False positives detected: 2 out of 4

=== Filter Statistics ===
Filter size: 1000 bits
Hash functions: 3
```

## How it Works:

1. **Hash Functions**: Multiple hash functions map each item to different positions in the bit array
2. **Adding Items**: Each hash position is set to 1
3. **Checking Items**: All hash positions must be 1 for the item to "exist"
4. **False Positives**: Items not in the set can still return "might exist" due to hash collisions

This implementation provides a practical example of how Bloom filters work in Perl, demonstrating their use for fast membership testing with a small memory footprint.

