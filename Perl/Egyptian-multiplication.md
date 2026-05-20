# Egyptian Multiplication Algorithm in Perl

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, is an ancient method of multiplying two numbers using repeated doubling and addition.

```perl
sub egyptian_multiply {
    my ($a, $b) = @_;
    
    # Handle edge cases
    return 0 if $a == 0 || $b == 0;
    
    # Ensure $a is the smaller number for efficiency
    if ($a > $b) {
        ($a, $b) = ($b, $a);
    }
    
    my $result = 0;
    
    # Main algorithm loop
    while ($a > 0) {
        # If $a is odd, add $b to the result
        if ($a % 2 == 1) {
            $result += $b;
        }
        
        # Double $b and halve $a
        $b *= 2;
        $a = int($a / 2);
    }
    
    return $result;
}

# Example usage
my $num1 = 17;
my $num2 = 23;

my $product = egyptian_multiply($num1, $num2);
print "Egyptian multiplication of $num1 × $num2 = $product\n";

# Let's trace through the steps for 17 × 23
print "\nStep-by-step trace for 17 × 23:\n";
print "17 × 23\n";

my ($a, $b) = (17, 23);
my $step = 1;

while ($a > 0) {
    if ($a % 2 == 1) {
        print "Step $step: $a is odd, add $b to result\n";
    } else {
        print "Step $step: $a is even, skip\n";
    }
    $a = int($a / 2);
    $b *= 2;
    $step++;
}
```

## How it works:

1. **Initialize**: Start with two numbers to multiply
2. **Loop**: While the first number is greater than 0:
   - If the first number is odd, add the second number to the result
   - Double the second number
   - Halve the first number (integer division)
3. **Return**: The accumulated result

## Example trace for 17 × 23:

```
Step 1: 17 is odd, add 23 to result (result = 23)
Step 2: 8 is even, skip
Step 3: 4 is even, skip
Step 4: 2 is even, skip
Step 5: 1 is odd, add 368 to result (result = 23 + 368 = 391)
```

## Output:
```
Egyptian multiplication of 17 × 23 = 391
```

This algorithm is efficient with O(log n) time complexity and demonstrates how ancient civilizations performed multiplication using only addition and doubling operations.

