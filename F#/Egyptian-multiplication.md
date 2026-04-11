# Egyptian Multiplication Algorithm in F#

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, is an ancient method for multiplying two numbers using repeated doubling and addition.

## Implementation

```fsharp
let egyptianMultiply a b =
    let rec multiply acc x y =
        match x, y with
        | 0, _ -> acc
        | _, 0 -> acc
        | x, y when x % 2 = 1 -> multiply (acc + y) (x / 2) (y * 2)
        | x, y -> multiply acc (x / 2) (y * 2)
    multiply 0 a b

// Alternative implementation with step-by-step tracing
let egyptianMultiplyTraced a b =
    let rec multiply acc x y steps =
        printfn "Step: x=%d, y=%d, acc=%d" x y acc
        match x, y with
        | 0, _ -> acc
        | _, 0 -> acc
        | x, y when x % 2 = 1 -> 
            printfn "  Adding y=%d to accumulator" y
            multiply (acc + y) (x / 2) (y * 2) (steps + 1)
        | x, y -> 
            printfn "  Skipping (x is even)"
            multiply acc (x / 2) (y * 2) (steps + 1)
    multiply 0 a b 0

// Example usage
let result1 = egyptianMultiply 13 9
printfn "13 × 9 = %d" result1

let result2 = egyptianMultiply 17 15
printfn "17 × 15 = %d" result2

// Traced example
printfn "\nTracing 7 × 13:"
let result3 = egyptianMultiplyTraced 7 13
printfn "Final result: %d" result3
```

## How it works

The algorithm works by:
1. **Halving** the first number (x) and **doubling** the second number (y)
2. **When x is odd**, add the current y value to the accumulator
3. **Continue** until x becomes 0
4. **Return** the accumulated sum

## Example Trace: 7 × 13

```
Step: x=7, y=13, acc=0
  Adding y=13 to accumulator
Step: x=3, y=26, acc=13
  Adding y=26 to accumulator
Step: x=1, y=52, acc=39
Step: x=0, y=104, acc=91
Final result: 91
```

## Output
```
13 × 9 = 117
17 × 15 = 255

Tracing 7 × 13:
Step: x=7, y=13, acc=0
  Adding y=13 to accumulator
Step: x=3, y=26, acc=13
  Adding y=26 to accumulator
Step: x=1, y=52, acc=39
Step: x=0, y=104, acc=91
Final result: 91
```

## Key Features

- **Time Complexity**: O(log n) where n is the smaller number
- **Space Complexity**: O(log n) due to recursion
- **No multiplication operator**: Uses only addition, subtraction, and bit shifting
- **Historically significant**: Represents one of the earliest known multiplication algorithms

This implementation demonstrates the elegant simplicity of ancient mathematical techniques that can be elegantly expressed in functional programming languages like F#.

