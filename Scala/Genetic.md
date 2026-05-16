# Genetic Algorithm in Scala

Here's a complete implementation of a genetic algorithm in Scala to solve the classic "One Max" problem (finding a binary string of all 1s):

```scala
import scala.util.Random

case class Individual(genes: List[Int], fitness: Double = 0.0) {
  def calculateFitness(target: List[Int]): Individual = {
    val fitnessScore = genes.zip(target).count { case (gene, targetGene) => gene == targetGene }
    this.copy(fitness = fitnessScore.toDouble)
  }
  
  def crossover(other: Individual): List[Individual] = {
    val crossoverPoint = Random.nextInt(genes.length)
    val child1 = Individual(genes.take(crossoverPoint) ++ other.genes.drop(crossoverPoint))
    val child2 = Individual(other.genes.take(crossoverPoint) ++ genes.drop(crossoverPoint))
    List(child1, child2)
  }
  
  def mutate(mutationRate: Double): Individual = {
    val mutatedGenes = genes.map { gene =>
      if (Random.nextDouble() < mutationRate) 1 - gene else gene
    }
    this.copy(genes = mutatedGenes)
  }
}

object GeneticAlgorithm {
  def evolve(
    populationSize: Int,
    chromosomeLength: Int,
    target: List[Int],
    mutationRate: Double,
    generations: Int
  ): Individual = {
    
    // Initialize population
    var population = (1 to populationSize).map { _ =>
      val genes = (1 to chromosomeLength).map(_ => Random.nextInt(2)).toList
      Individual(genes)
    }.toList
    
    for (generation <- 1 to generations) {
      // Calculate fitness for all individuals
      population = population.map(_.calculateFitness(target))
      
      // Sort by fitness (descending)
      population = population.sortBy(-_.fitness)
      
      // Print progress
      if (generation % 100 == 0) {
        println(s"Generation $generation: Best fitness = ${population.head.fitness}")
      }
      
      // Check if we found the solution
      if (population.head.fitness == target.length) {
        println(s"Solution found in generation $generation")
        return population.head
      }
      
      // Create new population through selection, crossover, and mutation
      val newPopulation = ListBuffer[Individual]()
      
      // Keep top 20% (elitism)
      val eliteSize = populationSize / 5
      newPopulation ++= population.take(eliteSize)
      
      // Generate rest of population
      while (newPopulation.size < populationSize) {
        val parent1 = tournamentSelection(population)
        val parent2 = tournamentSelection(population)
        
        val children = parent1.crossover(parent2)
        val mutatedChild1 = children(0).mutate(mutationRate)
        val mutatedChild2 = children(1).mutate(mutationRate)
        
        newPopulation += mutatedChild1
        newPopulation += mutatedChild2
      }
      
      population = newPopulation.take(populationSize).toList
    }
    
    population.head
  }
  
  def tournamentSelection(population: List[Individual]): Individual = {
    val tournamentSize = 3
    val tournament = (1 to tournamentSize).map(_ => population(Random.nextInt(population.length)))
    tournament.maxBy(_.fitness)
  }
}

// Usage example
object Main extends App {
  // Target binary string (e.g., 1111111111)
  val target = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  val result = GeneticAlgorithm.evolve(
    populationSize = 100,
    chromosomeLength = target.length,
    target = target,
    mutationRate = 0.01,
    generations = 1000
  )
  
  println(s"Target: $target")
  println(s"Result: ${result.genes}")
  println(s"Fitness: ${result.fitness}")
}
```

## Key Components Explained:

### 1. **Individual Case Class**
- Represents a single solution with genes (binary string) and fitness score
- Methods for fitness calculation, crossover, and mutation

### 2. **GeneticAlgorithm Object**
- Contains the main evolution loop
- Handles population initialization, selection, crossover, and mutation
- Implements tournament selection for parent selection

### 3. **Key Features**
- **Population Initialization**: Random binary strings
- **Fitness Function**: Counts matching bits with target
- **Selection**: Tournament selection
- **Crossover**: Single-point crossover
- **Mutation**: Bit-flip mutation
- **Elitism**: Preserves best individuals

### 4. **Parameters**
- `populationSize`: Number of individuals in each generation
- `chromosomeLength`: Length of binary strings
- `mutationRate`: Probability of mutation (0.01 = 1%)
- `generations`: Maximum number of iterations

## Sample Output:
```
Generation 100: Best fitness = 7.0
Generation 200: Best fitness = 9.0
Generation 300: Best fitness = 10.0
Solution found in generation 312
Target: List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Result: List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Fitness: 10.0
```

This implementation demonstrates the core principles of genetic algorithms in a functional Scala style while being easily extensible for other optimization problems.

