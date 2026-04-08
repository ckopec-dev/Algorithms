# Vogel's Approximation Method in Scala

Vogel's Approximation Method (VAM) is a heuristic method for solving transportation problems in operations research. Here's an implementation in Scala:

```scala
import scala.collection.mutable

case class TransportationProblem(
    supply: Array[Int],
    demand: Array[Int],
    costs: Array[Array[Int]]
)

case class Solution(
    allocation: Array[Array[Int]],
    totalCost: Int
)

object VogelsApproximationMethod {
  
  def solve(problem: TransportationProblem): Solution = {
    val supply = problem.supply.clone()
    val demand = problem.demand.clone()
    val costs = problem.costs.map(_.clone())
    val allocation = Array.ofDim[Int](supply.length, demand.length)
    
    var totalCost = 0
    val supplyIndices = (0 until supply.length).toArray
    val demandIndices = (0 until demand.length).toArray
    
    while (supplyIndices.exists(_ => supply(_) > 0) && 
           demandIndices.exists(_ => demand(_) > 0)) {
      
      // Calculate penalties for each row and column
      val rowPenalties = calculateRowPenalties(costs, supply, demand)
      val colPenalties = calculateColPenalties(costs, supply, demand)
      
      // Find maximum penalty
      val maxRowPenalty = rowPenalties.max
      val maxColPenalty = colPenalties.max
      
      // Determine where to allocate
      if (maxRowPenalty >= maxColPenalty) {
        val rowIndex = rowPenalties.indexOf(maxRowPenalty)
        val colIndex = findMinimumCostIndex(costs(rowIndex), demand)
        allocate(supply, demand, allocation, costs, rowIndex, colIndex)
      } else {
        val colIndex = colPenalties.indexOf(maxColPenalty)
        val rowIndex = findMinimumCostIndex(costs.map(_(colIndex)), supply)
        allocate(supply, demand, allocation, costs, rowIndex, colIndex)
      }
    }
    
    // Calculate total cost
    for (i <- allocation.indices; j <- allocation(i).indices) {
      totalCost += allocation(i)(j) * problem.costs(i)(j)
    }
    
    Solution(allocation, totalCost)
  }
  
  private def calculateRowPenalties(
      costs: Array[Array[Int]], 
      supply: Array[Int], 
      demand: Array[Int]
  ): Array[Int] = {
    val penalties = new Array[Int](costs.length)
    
    for (i <- costs.indices) {
      if (supply(i) > 0) {
        val validCosts = costs(i).zipWithIndex
          .filter { case (_, j) => demand(j) > 0 }
          .map(_._1)
        
        if (validCosts.length >= 2) {
          val sorted = validCosts.sorted
          penalties(i) = sorted(1) - sorted(0)
        } else if (validCosts.length == 1) {
          penalties(i) = validCosts(0)
        } else {
          penalties(i) = 0
        }
      } else {
        penalties(i) = 0
      }
    }
    
    penalties
  }
  
  private def calculateColPenalties(
      costs: Array[Array[Int]], 
      supply: Array[Int], 
      demand: Array[Int]
  ): Array[Int] = {
    val penalties = new Array[Int](costs(0).length)
    
    for (j <- costs(0).indices) {
      val validCosts = costs.zipWithIndex
        .filter { case (_, i) => supply(i) > 0 }
        .map(_._1(j))
      
      if (validCosts.length >= 2) {
        val sorted = validCosts.sorted
        penalties(j) = sorted(1) - sorted(0)
      } else if (validCosts.length == 1) {
        penalties(j) = validCosts(0)
      } else {
        penalties(j) = 0
      }
    }
    
    penalties
  }
  
  private def findMinimumCostIndex(
      costs: Array[Int], 
      demand: Array[Int]
  ): Int = {
    val validCosts = costs.zipWithIndex
      .filter { case (_, j) => demand(j) > 0 }
      .map { case (cost, index) => (cost, index) }
      .sortBy(_._1)
    
    if (validCosts.nonEmpty) validCosts.head._2 else -1
  }
  
  private def allocate(
      supply: Array[Int], 
      demand: Array[Int], 
      allocation: Array[Array[Int]],
      costs: Array[Array[Int]],
      rowIndex: Int,
      colIndex: Int
  ): Unit = {
    val quantity = math.min(supply(rowIndex), demand(colIndex))
    allocation(rowIndex)(colIndex) = quantity
    supply(rowIndex) -= quantity
    demand(colIndex) -= quantity
    
    // If supply or demand is exhausted, set costs to infinity
    if (supply(rowIndex) == 0) {
      for (j <- costs(rowIndex).indices) {
        costs(rowIndex)(j) = Int.MaxValue
      }
    }
    
    if (demand(colIndex) == 0) {
      for (i <- costs.indices) {
        costs(i)(colIndex) = Int.MaxValue
      }
    }
  }
  
  def printSolution(solution: Solution, problem: TransportationProblem): Unit = {
    println("Transportation Allocation:")
    println("  " + (0 until problem.demand.length).mkString("  D"))
    for (i <- problem.supply.indices) {
      print(s"S${i} ")
      for (j <- problem.demand.indices) {
        print(s"${solution.allocation(i)(j)}  ")
      }
      println()
    }
    println(s"\nTotal Cost: ${solution.totalCost}")
  }
}

// Example usage
object Example extends App {
  // Example transportation problem
  val supply = Array(300, 400, 500)  // Supply at each source
  val demand = Array(250, 350, 400)  // Demand at each destination
  val costs = Array(
    Array(8, 6, 10),  // Costs from source 0
    Array(9, 12, 13), // Costs from source 1
    Array(14, 9, 16)  // Costs from source 2
  )
  
  val problem = TransportationProblem(supply, demand, costs)
  val solution = VogelsApproximationMethod.solve(problem)
  
  println("Original Problem:")
  println("Supply: " + supply.mkString(", "))
  println("Demand: " + demand.mkString(", "))
  println("Costs:")
  costs.foreach(row => println(row.mkString("  ")))
  println()
  
  VogelsApproximationMethod.printSolution(solution, problem)
}
```

## How it works:

1. **Initialization**: Create copies of supply, demand, and cost arrays
2. **Penalty Calculation**: For each row and column, calculate the penalty as the difference between the two smallest costs
3. **Selection**: Choose the row or column with the maximum penalty
4. **Allocation**: Allocate as much as possible to the cell with the minimum cost in the selected row/column
5. **Update**: Reduce supply/demand and mark exhausted cells
6. **Repeat**: Continue until all supply and demand are satisfied

## Key Features:

- **Heuristic Approach**: Provides near-optimal solutions quickly
- **Penalty Method**: Uses the difference between smallest costs to prioritize allocations
- **Efficient Implementation**: Handles the transportation problem systematically
- **Clear Output**: Shows allocation matrix and total cost

This implementation demonstrates the core principles of Vogel's Approximation Method while maintaining clean, functional Scala code structure.

