# Expectation-Maximization (EM) Algorithm in Scala

Here's a complete implementation of the EM algorithm for Gaussian Mixture Models (GMM) in Scala:

```scala
import scala.math._
import scala.util.Random

// Data point case class
case class Point(x: Double, y: Double)

// Gaussian distribution model
case class Gaussian(mean: (Double, Double), covariance: (Double, Double, Double, Double), weight: Double) {
  def pdf(point: Point): Double = {
    val (x, y) = (point.x, point.y)
    val (muX, muY) = mean
    val (varX, covXY, covYX, varY) = (covariance._1, covariance._2, covariance._3, covariance._4)
    
    val det = varX * varY - covXY * covYX
    val invVarX = varY / det
    val invVarY = varX / det
    val invCovXY = -covXY / det
    
    val exponent = -0.5 * (
      invVarX * (x - muX) * (x - muX) +
      invVarY * (y - muY) * (y - muY) +
      2 * invCovXY * (x - muX) * (y - muY)
    )
    
    val normalization = 1.0 / sqrt(2 * Pi * Pi * det)
    normalization * exp(exponent)
  }
}

// EM Algorithm Implementation
class EMAlgorithm {
  
  // Initialize random Gaussians
  def initializeGaussians(points: Seq[Point], k: Int): Seq[Gaussian] = {
    val means = points.grouped(points.length / k).map { group =>
      val xs = group.map(_.x)
      val ys = group.map(_.y)
      (xs.sum / xs.length, ys.sum / ys.length)
    }.toList
    
    val covariances = (1 to k).map(_ => (1.0, 0.0, 0.0, 1.0)).toList
    
    means.zip(covariances).zipWithIndex.map { case ((mean, cov), i) =>
      Gaussian(mean, cov, 1.0 / k)
    }
  }
  
  // E-step: Calculate responsibilities
  def eStep(points: Seq[Point], gaussians: Seq[Gaussian]): Seq[Seq[Double]] = {
    points.map { point =>
      val weights = gaussians.map(_.weight)
      val pdfs = gaussians.map(_.pdf(point))
      val total = weights.zip(pdfs).map { case (w, p) => w * p }.sum
      
      if (total > 0) {
        weights.zip(pdfs).map { case (w, p) => (w * p) / total }
      } else {
        gaussians.map(_ => 0.0)
      }
    }
  }
  
  // M-step: Update parameters
  def mStep(points: Seq[Point], responsibilities: Seq[Seq[Double]], gaussians: Seq[Gaussian]): Seq[Gaussian] = {
    val k = gaussians.length
    val n = points.length
    
    val newWeights = responsibilities.transpose.map(_.sum / n)
    
    val newMeans = (0 until k).map { i =>
      val weightedSumX = points.zip(responsibilities).map { case (point, resp) => resp(i) * point.x }.sum
      val weightedSumY = points.zip(responsibilities).map { case (point, resp) => resp(i) * point.y }.sum
      val weightSum = responsibilities.transpose(i).sum
      
      if (weightSum > 0) {
        (weightedSumX / weightSum, weightedSumY / weightSum)
      } else {
        gaussians(i).mean
      }
    }
    
    val newCovariances = (0 until k).map { i =>
      val weightSum = responsibilities.transpose(i).sum
      if (weightSum > 0) {
        val mean = newMeans(i)
        val covX = points.zip(responsibilities).map { case (point, resp) =>
          val diffX = point.x - mean._1
          resp(i) * diffX * diffX
        }.sum / weightSum
        
        val covY = points.zip(responsibilities).map { case (point, resp) =>
          val diffY = point.y - mean._2
          resp(i) * diffY * diffY
        }.sum / weightSum
        
        val covXY = points.zip(responsibilities).map { case (point, resp) =>
          val diffX = point.x - mean._1
          val diffY = point.y - mean._2
          resp(i) * diffX * diffY
        }.sum / weightSum
        
        (covX, covXY, covXY, covY)
      } else {
        gaussians(i).covariance
      }
    }
    
    newWeights.zip(newMeans.zip(newCovariances)).map { case (weight, (mean, cov)) =>
      Gaussian(mean, cov, weight)
    }
  }
  
  // Main EM algorithm
  def run(points: Seq[Point], k: Int, maxIterations: Int = 100, tolerance: Double = 1e-6): Seq[Gaussian] = {
    var gaussians = initializeGaussians(points, k)
    var prevLogLikelihood = Double.MinValue
    
    for (iteration <- 1 to maxIterations) {
      val responsibilities = eStep(points, gaussians)
      gaussians = mStep(points, responsibilities, gaussians)
      
      // Calculate log likelihood
      val logLikelihood = points.map { point =>
        val likelihood = gaussians.map(g => g.weight * g.pdf(point)).sum
        if (likelihood > 0) log(likelihood) else -1000.0
      }.sum
      
      // Check for convergence
      if (abs(logLikelihood - prevLogLikelihood) < tolerance) {
        println(s"Converged at iteration $iteration")
        return gaussians
      }
      
      prevLogLikelihood = logLikelihood
    }
    
    gaussians
  }
}

// Example usage
object EMExample extends App {
  // Generate sample data
  val random = new Random(42)
  
  // Create 3 clusters of points
  val cluster1 = (1 to 50).map(_ => Point(random.nextGaussian() + 2, random.nextGaussian() + 2))
  val cluster2 = (1 to 50).map(_ => Point(random.nextGaussian() - 2, random.nextGaussian() - 2))
  val cluster3 = (1 to 50).map(_ => Point(random.nextGaussian() + 3, random.nextGaussian() - 3))
  
  val allPoints = cluster1 ++ cluster2 ++ cluster3
  
  // Run EM algorithm
  val em = new EMAlgorithm()
  val result = em.run(allPoints, k = 3, maxIterations = 50)
  
  // Print results
  println("Final Gaussian parameters:")
  result.zipWithIndex.foreach { case (gaussian, i) =>
    println(s"Cluster $i:")
    println(s"  Mean: (${gaussian.mean._1}, ${gaussian.mean._2})")
    println(s"  Weight: ${gaussian.weight}")
    println(s"  Covariance: (${gaussian.covariance._1}, ${gaussian.covariance._2}, ${gaussian.covariance._3}, ${gaussian.covariance._4})")
    println()
  }
}
```

## Key Components Explained

### 1. **Gaussian Model**
- Represents a 2D Gaussian distribution with mean, covariance matrix, and weight
- Includes PDF calculation for probability density

### 2. **E-step (Expectation)**
- Calculates responsibilities (posterior probabilities) of each point belonging to each cluster
- Uses current parameter estimates to compute how likely each point is from each Gaussian

### 3. **M-step (Maximization)**
- Updates Gaussian parameters (mean, covariance, weight) based on responsibilities
- Uses weighted moments to compute new parameters

### 4. **Main Algorithm Flow**
1. Initialize random Gaussian parameters
2. E-step: Compute responsibilities
3. M-step: Update parameters
4. Repeat until convergence or max iterations

## Usage Example

The example creates 3 clusters of 2D points and uses EM to find the underlying Gaussian mixture model. The algorithm will converge to parameters that best fit the data distribution.

## Output Format

The algorithm outputs the final estimated parameters for each Gaussian component including:
- Mean coordinates (x, y)
- Weight (mixing coefficient)
- Covariance matrix elements

This implementation demonstrates the core principles of the EM algorithm for unsupervised learning of mixture models.

