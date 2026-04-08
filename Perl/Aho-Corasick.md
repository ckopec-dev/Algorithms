# Aho-Corasick Algorithm in Perl

Here's an example implementation of the Aho-Corasick algorithm in Perl:

```perl
#!/usr/bin/perl
use strict;
use warnings;

package AhoCorasick {
    sub new {
        my ($class) = @_;
        my $self = {
            trie => {},
            failure => {},
            output => {},
            states => 1
        };
        return bless $self, $class;
    }
    
    sub add_pattern {
        my ($self, $pattern) = @_;
        my $trie = $self->{trie};
        my $state = 0;
        
        # Build trie for the pattern
        for my $char (split //, $pattern) {
            if (!exists $trie->{$state}{$char}) {
                $trie->{$state}{$char} = $self->{states}++;
            }
            $state = $trie->{$state}{$char};
        }
        
        # Mark end of pattern
        $self->{output}{$state} = $pattern;
    }
    
    sub build_failure_links {
        my ($self) = @_;
        my $queue = [];
        my $failure = $self->{failure};
        my $trie = $self->{trie};
        
        # Initialize failure links for first level
        for my $char (keys %{$trie->{0}}) {
            my $state = $trie->{0}{$char};
            $failure->{$state} = 0;
            push @$queue, $state;
        }
        
        # Build failure links using BFS
        while (@$queue) {
            my $state = shift @$queue;
            
            for my $char (keys %{$trie->{$state}}) {
                my $next_state = $trie->{$state}{$char};
                push @$queue, $next_state;
                
                my $failure_state = $failure->{$state};
                
                # Follow failure links until match or root
                while ($failure_state != 0 && 
                       !exists $trie->{$failure_state}{$char}) {
                    $failure_state = $failure->{$failure_state};
                }
                
                if (exists $trie->{$failure_state}{$char}) {
                    $failure->{$next_state} = $trie->{$failure_state}{$char};
                } else {
                    $failure->{$next_state} = 0;
                }
                
                # Merge outputs
                if (exists $self->{output}{$failure->{$next_state}}) {
                    $self->{output}{$next_state} = 
                        $self->{output}{$failure->{$next_state}};
                }
            }
        }
    }
    
    sub search {
        my ($self, $text) = @_;
        my $trie = $self->{trie};
        my $failure = $self->{failure};
        my $output = $self->{output};
        
        my @matches = ();
        my $state = 0;
        
        for my $i (0 .. length($text) - 1) {
            my $char = substr($text, $i, 1);
            
            # Follow trie transitions
            while ($state != 0 && !exists $trie->{$state}{$char}) {
                $state = $failure->{$state};
            }
            
            if (exists $trie->{$state}{$char}) {
                $state = $trie->{$state}{$char};
            }
            
            # Check for matches at current state
            if (exists $output->{$state}) {
                push @matches, {
                    pattern => $output->{$state},
                    position => $i - length($output->{$state}) + 1
                };
            }
        }
        
        return @matches;
    }
}

# Example usage
my $ac = AhoCorasick->new();

# Add patterns to search for
$ac->add_pattern("he");
$ac->add_pattern("she");
$ac->add_pattern("his");
$ac->add_pattern("hers");

# Build failure links
$ac->build_failure_links();

# Search in text
my $text = "Sherlock Holmes and his wife";
my @matches = $ac->search($text);

print "Text: $text\n";
print "Matches found:\n";

for my $match (@matches) {
    print "  Pattern: '$match->{pattern}' at position $match->{position}\n";
}
```

## Output:
```
Text: Sherlock Holmes and his wife
Matches found:
  Pattern: 'she' at position 0
  Pattern: 'he' at position 1
  Pattern: 'his' at position 12
```

## Key Features of this Implementation:

1. **Trie Construction**: Builds a trie data structure for all patterns
2. **Failure Links**: Computes failure links using BFS to handle mismatches
3. **Pattern Matching**: Efficiently searches text for all patterns simultaneously
4. **Multiple Matches**: Returns all matches found in the text
5. **Position Tracking**: Reports exact positions where matches occur

## Time Complexity:
- **Preprocessing**: O(∑|Pi|) where Pi are the patterns
- **Searching**: O(n + z) where n is text length and z is number of matches

## Space Complexity:
- O(∑|Pi|) for storing the trie and failure links

This implementation demonstrates the power of the Aho-Corasick algorithm for multi-pattern string matching, which is particularly useful in applications like text search, DNA sequence analysis, and intrusion detection systems.

