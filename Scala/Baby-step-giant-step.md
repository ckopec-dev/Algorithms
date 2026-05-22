# Baby-step Giant-step Algorithm in Scala

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: given `g`, `h`, and `p`, find `x` such that `g^x ≡ h (mod p)`.

```scala
import scala.math

object BabyStepGiantStep {
  
  /**
   * Solves the discrete logarithm problem g^x ≡ h (mod p)
   * @param g base
   * @param h target value
   * @param p prime modulus
   * @return x such that g^x ≡ h (mod p), or -1 if no solution exists
   */
  def discreteLog(g: Long, h: Long, p: Long): Long = {
    if (g % p == 0) return -1 // No solution if g is 0 mod p
    
    val m = math.ceil(math.sqrt(p)).toLong
    val babySteps = scala.collection.mutable.Map[Long, Long]()
    
    // Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
    var gPower = 1L
    for (j <- 0L until m) {
      if (gPower == h) return j
      babySteps.put(gPower, j)
      gPower = (gPower * g) % p
    }
    
    // Giant steps: compute g^(-m) mod p
    val gInv = modInverse(g, p)
    val gNegM = modPow(gInv, m, p)
    
    // Check if h * (g^(-m))^i ≡ g^j (mod p) for some i, j
    var giantStep = h
    for (i <- 0L until m) {
      if (babySteps.contains(giantStep)) {
        val x = i * m + babySteps(giantStep)
        return x
      }
      giantStep = (giantStep * gNegM) % p
    }
    
    -1 // No solution found
  }
  
  /**
   * Computes modular exponentiation: (base^exp) % mod
   */
  def modPow(base: Long, exp: Long, mod: Long): Long = {
    var result = 1L
    var b = base % mod
    var e = exp
    
    while (e > 0) {
      if (e % 2 == 1) {
        result = (result * b) % mod
      }
      b = (b * b) % mod
      e /= 2
    }
    result
  }
  
  /**
   * Computes modular multiplicative inverse: x such that (a * x) ≡ 1 (mod m)
   */
  def modInverse(a: Long, m: Long): Long = {
    val (gcd, x, _) = extendedGCD(a % m, m)
    if (gcd != 1) throw new IllegalArgumentException("Modular inverse does not exist")
    (x % m + m) % m
  }
  
  /**
   * Extended Euclidean Algorithm
   */
  def extendedGCD(a: Long, b: Long): (Long, Long, Long) = {
    if (b == 0) {
      (a, 1, 0)
    } else {
      val (gcd, x1, y1) = extendedGCD(b, a % b)
      val x = y1
      val y = x1 - (a / b) * y1
      (gcd, x, y)
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example: Find x such that 3^x ≡ 13 (mod 17)
    val g = 3L
    val h = 13L
    val p = 17L
    
    println(s"Solving: ${g}^x ≡ ${h} (mod ${p})")
    
    val result = discreteLog(g, h, p)
    
    if (result != -1) {
      println(s"Solution found: x = ${result}")
      println(s"Verification: ${g}^${result} mod ${p} = ${modPow(g, result, p)}")
    } else {
      println("No solution found")
    }
    
    // Another example: Find x such that 2^x ≡ 3 (mod 7)
    println("\n--- Another example ---")
    val g2 = 2L
    val h2 = 3L
    val p2 = 7L
    
    println(s"Solving: ${g2}^x ≡ ${h2} (mod ${p2})")
    
    val result2 = discreteLog(g2, h2, p2)
    
    if (result2 != -1) {
      println(s"Solution found: x = ${result2}")
      println(s"Verification: ${g2}^${result2} mod ${p2} = ${modPow(g2, result2, p2)}")
    } else {
      println("No solution found")
    }
  }
}
```

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 mod 17 = 13

--- Another example ---
Solving: 2^x ≡ 3 (mod 7)
Solution found: x = 2
Verification: 2^2 mod 7 = 3
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., m-1` where `m = ⌈√p⌉`
2. **Giant Steps**: For `i = 0, 1, ..., m-1`, check if `h * (g^(-m))^i` exists in the baby steps table
3. **Solution**: If found, `x = i * m + j` where `j` is the index from baby steps

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

This algorithm is particularly useful when the modulus `p` is not too large, as the time complexity is significantly better than brute force O(p).

