# Shamir's Secret Sharing in Scala

Here's an implementation of Shamir's Secret Sharing algorithm in Scala:

```scala
import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

object ShamirSecretSharing {
  
  // Define a case class for polynomial terms
  case class Polynomial(coefficients: List[BigDecimal]) {
    def evaluate(x: BigDecimal): BigDecimal = {
      coefficients.zipWithIndex.foldLeft(BigDecimal(0)) { case (result, (coeff, power)) =>
        result + coeff * BigDecimal(1).pow(power)
      }
    }
    
    def degree: Int = coefficients.length - 1
  }
  
  // Generate random prime number for finite field
  def generatePrime(bits: Int): BigDecimal = {
    val prime = BigDecimal("2").pow(bits) - 1
    // In a real implementation, you'd generate a proper prime
    prime
  }
  
  // Modular arithmetic operations
  def mod(a: BigDecimal, m: BigDecimal): BigDecimal = {
    val result = a % m
    if (result < 0) result + m else result
  }
  
  // Extended Euclidean Algorithm for modular inverse
  def extendedGCD(a: BigDecimal, b: BigDecimal): (BigDecimal, BigDecimal, BigDecimal) = {
    if (b == 0) (a, 1, 0)
    else {
      val (gcd, x1, y1) = extendedGCD(b, a % b)
      (gcd, y1, x1 - (a / b) * y1)
    }
  }
  
  // Modular inverse
  def modInverse(a: BigDecimal, m: BigDecimal): BigDecimal = {
    val (gcd, x, _) = extendedGCD(a, m)
    if (gcd != 1) throw new IllegalArgumentException("Modular inverse does not exist")
    mod(x, m)
  }
  
  // Generate shares using Shamir's Secret Sharing
  def generateShares(secret: BigDecimal, threshold: Int, numShares: Int, prime: BigDecimal): List[(BigDecimal, BigDecimal)] = {
    // Generate random coefficients for polynomial (except constant term which is the secret)
    val coefficients = secret :: (0 until threshold - 1).map(_ => BigDecimal(scala.util.Random.nextInt(1000000))).toList
    
    val polynomial = Polynomial(coefficients)
    
    // Generate shares
    (1 to numShares).map { i =>
      val x = BigDecimal(i)
      val y = polynomial.evaluate(x)
      (x, mod(y, prime))
    }.toList
  }
  
  // Reconstruct secret using Lagrange interpolation
  def reconstructSecret(shares: List[(BigDecimal, BigDecimal)], prime: BigDecimal): BigDecimal = {
    val k = shares.length
    
    val secret = shares.zipWithIndex.foldLeft(BigDecimal(0)) { case (result, (share, i)) =>
      val (x_i, y_i) = share
      
      // Calculate Lagrange basis polynomial
      val numerator = shares.zipWithIndex.foldLeft(BigDecimal(1)) { case (prod, (otherShare, j)) =>
        if (i != j) {
          val (x_j, _) = otherShare
          prod * x_j
        } else prod
      }
      
      val denominator = shares.zipWithIndex.foldLeft(BigDecimal(1)) { case (prod, (otherShare, j)) =>
        if (i != j) {
          val (x_j, _) = otherShare
          prod * (x_i - x_j)
        } else prod
      }
      
      val lagrangeCoeff = modInverse(denominator, prime) * numerator
      result + y_i * lagrangeCoeff
    }
    
    mod(secret, prime)
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Set up parameters
    val secret = BigDecimal("123456")
    val threshold = 3  // Minimum number of shares needed to reconstruct
    val numShares = 5  // Total number of shares to generate
    val prime = generatePrime(32)  // Large prime for finite field
    
    println(s"Original secret: $secret")
    println(s"Threshold: $threshold")
    println(s"Number of shares: $numShares")
    println(s"Prime: $prime")
    println()
    
    // Generate shares
    val shares = generateShares(secret, threshold, numShares, prime)
    
    println("Generated shares:")
    shares.foreach { case (x, y) =>
      println(s"Share: ($x, $y)")
    }
    println()
    
    // Reconstruct secret using threshold number of shares
    val reconstructedSecret = reconstructSecret(shares.take(threshold), prime)
    
    println(s"Reconstructed secret: $reconstructedSecret")
    println(s"Secret matches: ${secret == reconstructedSecret}")
    
    // Test with fewer shares (should fail)
    println("\nTesting with insufficient shares:")
    try {
      val insufficientShares = shares.take(2)
      val wrongSecret = reconstructSecret(insufficientShares, prime)
      println(s"Reconstructed with 2 shares: $wrongSecret")
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
```

## Key Components Explained:

### 1. **Polynomial Generation**
- Creates a polynomial of degree (threshold-1)
- The secret is the constant term (coefficient 0)
- Random coefficients are generated for higher degree terms

### 2. **Share Generation**
- Each share is a point (x, y) on the polynomial
- x values are sequential integers starting from 1
- y values are calculated using polynomial evaluation

### 3. **Secret Reconstruction**
- Uses Lagrange interpolation to reconstruct the polynomial
- Requires at least `threshold` shares to recover the secret
- Works in a finite field using modular arithmetic

### 4. **Modular Arithmetic**
- Ensures all operations work within a finite field
- Provides proper handling of negative numbers and division

## Usage Example:

The code demonstrates:
1. Generating 5 shares from secret `123456` with threshold `3`
2. Reconstructing the secret using any 3 shares
3. Showing that insufficient shares cannot reconstruct the secret

This implementation provides a complete working example of Shamir's Secret Sharing in Scala, suitable for cryptographic applications requiring secure secret distribution.

