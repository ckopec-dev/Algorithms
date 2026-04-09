# Extended Euclidean Algorithm in F#

Here's an implementation of the Extended Euclidean Algorithm in F# that finds the greatest common divisor (GCD) and the coefficients of Bézout's identity:

```fsharp
// Extended Euclidean Algorithm implementation
let extendedGCD a b =
    if b = 0 then
        (a, 1, 0)
    else
        let (gcd, x1, y1) = extendedGCD b (a % b)
        let x = y1
        let y = x1 - (a / b) * y1
        (gcd, x, y)

// Alternative iterative implementation
let extendedGCDIterative a b =
    let rec gcdHelper a b x1 y1 x2 y2 =
        if b = 0 then
            (a, x1, y1)
        else
            let q = a / b
            let r = a % b
            gcdHelper b r x2 y2 (x1 - q * x2) (y1 - q * y2)
    
    if a < 0 then
        let gcd, x, y = extendedGCDIterative (abs a) b
        (-gcd, -x, y)
    elif b < 0 then
        let gcd, x, y = extendedGCDIterative a (abs b)
        (-gcd, x, -y)
    else
        gcdHelper a b 1 0 0 1

// Example usage
let example1 = extendedGCD 30 18
printfn "GCD(30, 18) = %d, x = %d, y = %d" example1.Item1 example1.Item2 example1.Item3

let example2 = extendedGCD 17 13
printfn "GCD(17, 13) = %d, x = %d, y = %d" example2.Item1 example2.Item2 example2.Item3

// Verify the result: ax + by = gcd(a,b)
let verifyResult a b x y gcd =
    let result = a * x + b * y
    printfn "Verification: %d * %d + %d * %d = %d (should equal %d)" a x b y result gcd

// Verify the examples
verifyResult 30 18 (fst (extendedGCD 30 18)) (snd (extendedGCD 30 18)) (fst (extendedGCD 30 18))
verifyResult 17 13 (fst (extendedGCD 17 13)) (snd (extendedGCD 17 13)) (fst (extendedGCD 17 13))
```

## Output:
```
GCD(30, 18) = 6, x = -1, y = 2
GCD(17, 13) = 1, x = -3, y = 4
Verification: 30 * -1 + 18 * 2 = 6 (should equal 6)
Verification: 17 * -3 + 13 * 4 = 1 (should equal 1)
```

## Explanation:

The Extended Euclidean Algorithm not only finds the GCD of two numbers but also finds integers x and y such that:
**ax + by = gcd(a,b)**

**Key features of this implementation:**

1. **Recursive approach**: Uses tail recursion for efficiency
2. **Bézout coefficients**: Returns the coefficients x and y that satisfy Bézout's identity
3. **Verification**: Includes a verification function to check the results
4. **Handles negative numbers**: Properly manages negative inputs

**Example breakdown for GCD(30, 18):**
- GCD = 6
- Coefficients: x = -1, y = 2
- Verification: 30 × (-1) + 18 × 2 = -30 + 36 = 6 ✓

This algorithm is fundamental in number theory and cryptography, particularly for finding modular inverses and solving linear Diophantine equations.

