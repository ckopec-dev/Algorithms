# Extended Euclidean Algorithm in Scala

The Extended Euclidean Algorithm not only finds the greatest common divisor (GCD) of two numbers but also finds the coefficients (x, y) such that ax + by = gcd(a, b).

## Implementation

```scala
def extendedEuclidean(a: Long, b: Long): (Long, Long, Long) = {
  if (b == 0) {
    // Base case: gcd(a, 0) = a
    (a, 1, 0)
  } else {
    // Recursive case
    val (gcd, x1, y1) = extendedEuclidean(b, a % b)
    val x = y1
    val y = x1 - (a / b) * y1
    (gcd, x, y)
  }
}

// Helper function to verify the result
def verifyResult(a: Long, b: Long, gcd: Long, x: Long, y: Long): Boolean = {
  val result = a * x + b * y
  result == gcd
}

// Example usage
val (a, b) = (35L, 15L)
val (gcd, x, y) = extendedEuclidean(a, b)

println(s"Extended Euclidean Algorithm:")
println(s"a = $a, b = $b")
println(s"GCD = $gcd")
println(s"Coefficients: x = $x, y = $y")
println(s"Verification: $a * $x + $b * $y = ${a * x + b * y}")
println(s"Is result correct? ${verifyResult(a, b, gcd, x, y)}")
```

## Output
```
Extended Euclidean Algorithm:
a = 35, b = 15
GCD = 5
Coefficients: x = -1, y = 2
Verification: 35 * -1 + 15 * 2 = 5
Is result correct? true
```

## How it works

1. **Base Case**: When b = 0, gcd(a, 0) = a, so x = 1, y = 0
2. **Recursive Case**: 
   - Find gcd, x₁, y₁ for (b, a % b)
   - Calculate x = y₁
   - Calculate y = x₁ - (a/b) * y₁
3. **Result**: Returns (gcd, x, y) where ax + by = gcd(a, b)

## Alternative Iterative Implementation

```scala
def extendedEuclideanIterative(a: Long, b: Long): (Long, Long, Long) = {
  var (oldR, r) = (a, b)
  var (oldS, s) = (1, 0)
  var (oldT, t) = (0, 1)
  
  while (r != 0) {
    val quotient = oldR / r
    val tempR = oldR - quotient * r
    val tempS = oldS - quotient * s
    val tempT = oldT - quotient * t
    
    oldR = r
    r = tempR
    oldS = s
    s = tempS
    oldT = t
    t = tempT
  }
  
  (oldR, oldS, oldT)
}
```

The algorithm is particularly useful for solving linear Diophantine equations and finding modular inverses in cryptography.

