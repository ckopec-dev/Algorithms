# Cocke-Younger-Kasami (CYK) Parsing Algorithm in Scala

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form. Here's a complete implementation in Scala:

```scala
import scala.collection.mutable

// Grammar rule representation
case class Rule(lhs: String, rhs: List[String])

// CYK parser implementation
class CYKParser(grammar: List[Rule], startSymbol: String) {
  
  // Convert grammar to Chomsky Normal Form (CNF) if needed
  def isCNF: Boolean = {
    grammar.forall { rule =>
      rule.rhs.length match {
        case 0 => false // Empty production not allowed in CNF
        case 1 => rule.rhs.head.matches("^[A-Z]$") // Terminal symbol
        case 2 => rule.rhs.forall(_.matches("^[A-Z]$")) // Non-terminal symbols
        case _ => false // CNF allows only 0, 1, or 2 symbols on RHS
      }
    }
  }
  
  // Parse a string using CYK algorithm
  def parse(input: String): Boolean = {
    if (input.isEmpty) return true
    
    val n = input.length
    val table = Array.ofDim[mutable.Set[String]](n, n)
    
    // Initialize the first diagonal
    for (i <- 0 until n) {
      val char = input(i).toString
      val terminals = grammar.filter(_.rhs.contains(char)).map(_.lhs)
      table(i)(0) = mutable.Set.empty[String]
      table(i)(0).addAll(terminals)
    }
    
    // Fill the table using dynamic programming
    for (length <- 2 to n) {
      for (i <- 0 to n - length) {
        val j = i + length - 1
        table(i)(j) = mutable.Set.empty[String]
        
        // Try all possible splits
        for (k <- i + 1 to j) {
          val left = table(i)(k - 1)
          val right = table(k)(j)
          
          // Check all rules that can produce combinations of left and right
          for (rule <- grammar) {
            if (rule.rhs.length == 2) {
              val (A, B) = (rule.rhs(0), rule.rhs(1))
              if (left.contains(A) && right.contains(B)) {
                table(i)(j) += rule.lhs
              }
            }
          }
        }
      }
    }
    
    // Check if start symbol is in the top-right cell
    table(0)(n - 1).contains(startSymbol)
  }
  
  // Get detailed parsing table (for debugging)
  def getParseTable(input: String): Array[Array[mutable.Set[String]]] = {
    val n = input.length
    val table = Array.ofDim[mutable.Set[String]](n, n)
    
    // Initialize the first diagonal
    for (i <- 0 until n) {
      val char = input(i).toString
      val terminals = grammar.filter(_.rhs.contains(char)).map(_.lhs)
      table(i)(0) = mutable.Set.empty[String]
      table(i)(0).addAll(terminals)
    }
    
    // Fill the table
    for (length <- 2 to n) {
      for (i <- 0 to n - length) {
        val j = i + length - 1
        table(i)(j) = mutable.Set.empty[String]
        
        for (k <- i + 1 to j) {
          val left = table(i)(k - 1)
          val right = table(k)(j)
          
          for (rule <- grammar) {
            if (rule.rhs.length == 2) {
              val (A, B) = (rule.rhs(0), rule.rhs(1))
              if (left.contains(A) && right.contains(B)) {
                table(i)(j) += rule.lhs
              }
            }
          }
        }
      }
    }
    
    table
  }
}

// Example usage
object CYKExample extends App {
  
  // Define a simple grammar in Chomsky Normal Form
  // S -> AB | BC
  // A -> BA | a
  // B -> CC | b
  // C -> AB | a
  val grammar = List(
    Rule("S", List("A", "B")),
    Rule("S", List("B", "C")),
    Rule("A", List("B", "A")),
    Rule("A", List("a")),
    Rule("B", List("C", "C")),
    Rule("B", List("b")),
    Rule("C", List("A", "B")),
    Rule("C", List("a"))
  )
  
  val parser = new CYKParser(grammar, "S")
  
  // Test cases
  val testCases = List("ab", "aab", "baa", "bb", "abab")
  
  println("CYK Parsing Results:")
  println("====================")
  
  testCases.foreach { testCase =>
    val result = parser.parse(testCase)
    println(s"'$testCase': ${if (result) "ACCEPTED" else "REJECTED"}")
  }
  
  // Show detailed parsing for one example
  println("\nDetailed parsing for 'ab':")
  val table = parser.getParseTable("ab")
  println("Parsing table:")
  for (i <- table.indices) {
    for (j <- table(i).indices) {
      if (table(i)(j).nonEmpty) {
        print(s"[${table(i)(j).mkString(",")}] ")
      } else {
        print("[ ] ")
      }
    }
    println()
  }
}
```

## Key Features of this Implementation:

1. **Grammar Representation**: Uses `Rule` case class to represent grammar rules
2. **CNF Validation**: Checks if the grammar is in Chomsky Normal Form
3. **Dynamic Programming**: Implements the core CYK algorithm using a 2D table
4. **Parse Table Generation**: Provides detailed parsing information for debugging
5. **Flexible Input**: Handles strings of any length

## How it Works:

1. **Initialization**: Fill the diagonal of the parsing table with terminals
2. **Dynamic Programming**: For each substring length, compute possible non-terminals
3. **Rule Matching**: Check if grammar rules can generate combinations of existing symbols
4. **Final Check**: Verify if the start symbol appears in the top-right cell

## Time and Space Complexity:

- **Time Complexity**: O(n³ × |G|) where n is input length and |G| is grammar size
- **Space Complexity**: O(n² × |G|)

This implementation demonstrates the classic CYK algorithm for parsing context-free grammars, which is particularly useful for recognizing strings in languages defined by CFGs in CNF.

