# Euclidean Algorithm in Go

The Euclidean algorithm is used to find the Greatest Common Divisor (GCD) of two numbers.

## Iterative Implementation

```go
package main

import "fmt"

// gcdIterative calculates GCD using iterative approach
func gcdIterative(a, b int) int {
    for b != 0 {
        a, b = b, a%b
    }
    return a
}

func main() {
    // Example usage
    num1 := 48
    num2 := 18
    
    result := gcdIterative(num1, num2)
    fmt.Printf("GCD of %d and %d is: %d\n", num1, num2, result)
    
    // More examples
    fmt.Printf("GCD of 100 and 25 is: %d\n", gcdIterative(100, 25))
    fmt.Printf("GCD of 17 and 13 is: %d\n", gcdIterative(17, 13))
    fmt.Printf("GCD of 144 and 60 is: %d\n", gcdIterative(144, 60))
}
```

## Recursive Implementation

```go
package main

import "fmt"

// gcdRecursive calculates GCD using recursive approach
func gcdRecursive(a, b int) int {
    if b == 0 {
        return a
    }
    return gcdRecursive(b, a%b)
}

func main() {
    // Example usage
    num1 := 48
    num2 := 18
    
    result := gcdRecursive(num1, num2)
    fmt.Printf("GCD of %d and %d is: %d\n", num1, num2, result)
    
    // More examples
    fmt.Printf("GCD of 100 and 25 is: %d\n", gcdRecursive(100, 25))
    fmt.Printf("GCD of 17 and 13 is: %d\n", gcdRecursive(17, 13))
    fmt.Printf("GCD of 144 and 60 is: %d\n", gcdRecursive(144, 60))
}
```

## Complete Example with Step-by-Step Trace

```go
package main

import "fmt"

// gcdWithSteps shows the step-by-step process
func gcdWithSteps(a, b int) int {
    fmt.Printf("Finding GCD of %d and %d:\n", a, b)
    
    for b != 0 {
        quotient := a / b
        remainder := a % b
        fmt.Printf("  %d = %d × %d + %d\n", a, b, quotient, remainder)
        a, b = b, remainder
    }
    
    fmt.Printf("GCD is: %d\n", a)
    return a
}

func main() {
    fmt.Println("=== Euclidean Algorithm Demo ===")
    result := gcdWithSteps(48, 18)
    fmt.Printf("\nFinal result: GCD(48, 18) = %d\n", result)
}
```

## Output
```
=== Euclidean Algorithm Demo ===
Finding GCD of 48 and 18:
  48 = 18 × 2 + 12
  18 = 12 × 1 + 6
  12 = 6 × 2 + 0
GCD is: 6

Final result: GCD(48, 18) = 6
```

## Key Points

- **Time Complexity**: O(log(min(a, b)))
- **Space Complexity**: O(1) for iterative, O(log(min(a, b))) for recursive
- **Principle**: GCD(a, b) = GCD(b, a mod b)
- **Base Case**: When b = 0, GCD(a, 0) = a

The algorithm works by repeatedly applying the principle that the GCD of two numbers doesn't change if we replace the larger number with its difference with the smaller number, or more efficiently, with the remainder when dividing the larger by the smaller.

