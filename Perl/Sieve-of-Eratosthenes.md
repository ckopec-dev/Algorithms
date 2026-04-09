# Sieve of Eratosthenes in Perl

Here's an implementation of the Sieve of Eratosthenes algorithm in Perl to find all prime numbers up to a given limit:

```perl
sub sieve_of_eratosthenes {
    my ($limit) = @_;
    
    # Handle edge cases
    return () if $limit < 2;
    
    # Create array to track prime numbers
    # Index represents the number, value represents if it's prime
    my @is_prime = (1) x ($limit + 1);
    $is_prime[0] = $is_prime[1] = 0;  # 0 and 1 are not prime
    
    # Sieve process
    for my $i (2 .. int(sqrt($limit))) {
        if ($is_prime[$i]) {
            # Mark all multiples of $i as not prime
            for my $j ($i * $i .. $limit step $i) {
                $is_prime[$j] = 0;
            }
        }
    }
    
    # Collect all prime numbers
    my @primes = grep { $is_prime[$_] } 2 .. $limit;
    
    return @primes;
}

# Example usage
my $limit = 30;
my @primes = sieve_of_eratosthenes($limit);

print "Prime numbers up to $limit:\n";
print join(", ", @primes) . "\n";

# Output: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29
```

## How it works:

1. **Initialization**: Create a boolean array where each index represents a number, initially all set to `true` (prime)
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as `false` (not prime)
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Collection**: Gather all numbers that remain marked as prime

## Alternative compact version:

```perl
sub sieve_compact {
    my $n = shift;
    return () if $n < 2;
    
    my @p = (1) x ($n + 1);
    $p[0] = $p[1] = 0;
    
    for (2 .. int(sqrt($n))) {
        if ($p[$_]) {
            my $j = $_ * $_;
            $p[$j] = 0 while $j <= $n;
        }
    }
    
    return grep { $p[$_] } 2 .. $n;
}

# Test
print "Primes up to 20: " . join(", ", sieve_compact(20)) . "\n";
# Output: Primes up to 20: 2, 3, 5, 7, 11, 13, 17, 19
```

**Time Complexity**: O(n log log n)  
**Space Complexity**: O(n)

