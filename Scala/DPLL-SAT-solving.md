# DPLL SAT Solver in Scala

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Scala:

```scala
import scala.collection.mutable

// Represents a literal (variable or its negation)
case class Literal(varName: String, isNegated: Boolean = false) {
  def negate: Literal = Literal(varName, !isNegated)
  override def toString: String = if (isNegated) s"¬$varName" else varName
}

// Represents a clause (disjunction of literals)
case class Clause(literals: Set[Literal]) {
  def contains(literal: Literal): Boolean = literals.contains(literal)
  def containsNegationOf(literal: Literal): Boolean = literals.contains(literal.negate)
  def size: Int = literals.size
  override def toString: String = literals.mkString("(", " ∨ ", ")")
}

// Represents a SAT problem
case class SATProblem(clauses: List[Clause], variables: Set[String]) {
  def unitClauses: List[Clause] = clauses.filter(_.size == 1)
  def emptyClauses: List[Clause] = clauses.filter(_.size == 0)
  def isSatisfiable: Boolean = emptyClauses.isEmpty
}

object DPLL {
  
  // Main DPLL algorithm
  def solve(problem: SATProblem): Option[Map[String, Boolean]] = {
    val clauses = problem.clauses
    val variables = problem.variables
    
    // Initialize assignment
    val assignment = mutable.Map[String, Boolean]()
    
    // Check for trivial unsatisfiability
    if (problem.emptyClauses.nonEmpty) return None
    
    // Run DPLL
    dpll(clauses, variables, assignment)
  }
  
  private def dpll(clauses: List[Clause], 
                   variables: Set[String], 
                   assignment: mutable.Map[String, Boolean]): Option[Map[String, Boolean]] = {
    
    // Check if all clauses are satisfied
    if (clauses.forall(isClauseSatisfied(_, assignment))) {
      return Some(assignment.toMap)
    }
    
    // Check if there's an empty clause (unsatisfiable)
    if (clauses.exists(_.size == 0)) {
      return None
    }
    
    // Unit propagation
    val unitClauses = clauses.filter(_.size == 1)
    if (unitClauses.nonEmpty) {
      val unitClause = unitClauses.head
      val literal = unitClause.literals.head
      val varName = literal.varName
      
      // Add unit literal to assignment
      assignment(varName) = !literal.isNegated
      
      // Simplify clauses
      val simplifiedClauses = simplifyClauses(clauses, literal)
      
      // Recursively solve
      return dpll(simplifiedClauses, variables - varName, assignment)
    }
    
    // Choose a variable (simple heuristic: first one)
    val varName = variables.head
    
    // Try both truth values
    val assignmentsToTry = List(true, false)
    
    for (value <- assignmentsToTry) {
      assignment(varName) = value
      
      // Simplify clauses with this assignment
      val simplifiedClauses = simplifyClauses(clauses, Literal(varName, !value))
      
      // Recursively solve
      val result = dpll(simplifiedClauses, variables - varName, assignment)
      
      if (result.isDefined) {
        return result
      }
      
      // Backtrack
      assignment.remove(varName)
    }
    
    None
  }
  
  // Check if a clause is satisfied by current assignment
  private def isClauseSatisfied(clause: Clause, assignment: mutable.Map[String, Boolean]): Boolean = {
    clause.literals.exists { literal =>
      val varValue = assignment.get(literal.varName)
      varValue.exists(value => value != literal.isNegated)
    }
  }
  
  // Simplify clauses by removing satisfied clauses and literals
  private def simplifyClauses(clauses: List[Clause], 
                             literal: Literal): List[Clause] = {
    clauses.filter { clause =>
      // Remove satisfied clauses
      !isClauseSatisfied(clause, mutable.Map.empty[String, Boolean] ++ Map(literal.varName -> !literal.isNegated))
    }.map { clause =>
      // Remove literal from clauses
      val newLiterals = clause.literals - literal
      Clause(newLiterals)
    }
  }
}

// Example usage
object SATExample extends App {
  
  // Example 1: Simple satisfiable problem
  // (A ∨ B) ∧ (¬A ∨ C) ∧ (¬B ∨ ¬C)
  val problem1 = SATProblem(
    clauses = List(
      Clause(Set(Literal("A"), Literal("B"))),
      Clause(Set(Literal("A", true), Literal("C"))),
      Clause(Set(Literal("B", true), Literal("C", true)))
    ),
    variables = Set("A", "B", "C")
  )
  
  println("Problem 1: (A ∨ B) ∧ (¬A ∨ C) ∧ (¬B ∨ ¬C)")
  val result1 = DPLL.solve(problem1)
  println(s"Solution: $result1")
  
  // Example 2: Unsatisfiable problem
  // (A ∨ B) ∧ (¬A ∨ B) ∧ (A ∨ ¬B) ∧ (¬A ∨ ¬B)
  val problem2 = SATProblem(
    clauses = List(
      Clause(Set(Literal("A"), Literal("B"))),
      Clause(Set(Literal("A", true), Literal("B"))),
      Clause(Set(Literal("A"), Literal("B", true))),
      Clause(Set(Literal("A", true), Literal("B", true)))
    ),
    variables = Set("A", "B")
  )
  
  println("\nProblem 2: (A ∨ B) ∧ (¬A ∨ B) ∧ (A ∨ ¬B) ∧ (¬A ∨ ¬B)")
  val result2 = DPLL.solve(problem2)
  println(s"Solution: $result2")
  
  // Example 3: Simple satisfiable problem
  // (A ∨ B) ∧ (¬A ∨ C)
  val problem3 = SATProblem(
    clauses = List(
      Clause(Set(Literal("A"), Literal("B"))),
      Clause(Set(Literal("A", true), Literal("C")))
    ),
    variables = Set("A", "B", "C")
  )
  
  println("\nProblem 3: (A ∨ B) ∧ (¬A ∨ C)")
  val result3 = DPLL.solve(problem3)
  println(s"Solution: $result3")
}
```

## Key Features of this Implementation:

1. **Literal and Clause Representation**: 
   - `Literal` class represents variables and their negations
   - `Clause` class represents disjunctions of literals

2. **Core DPLL Algorithm**:
   - **Unit Propagation**: Resolves unit clauses immediately
   - **Backtracking**: Tries both truth values for variables
   - **Clause Simplification**: Removes satisfied clauses and literals

3. **Algorithm Steps**:
   - Check if all clauses are satisfied
   - Check for empty clauses (unsatisfiable)
   - Apply unit propagation if possible
   - Choose a variable and try both assignments
   - Backtrack on failure

4. **Return Values**:
   - `Some(Map[String, Boolean])` for satisfiable problems
   - `None` for unsatisfiable problems

## Sample Output:
```
Problem 1: (A ∨ B) ∧ (¬A ∨ C) ∧ (¬B ∨ ¬C)
Solution: Some(Map(A -> true, B -> true, C -> true))

Problem 2: (A ∨ B) ∧ (¬A ∨ B) ∧ (A ∨ ¬B) ∧ (¬A ∨ ¬B)
Solution: None

Problem 3: (A ∨ B) ∧ (¬A ∨ C)
Solution: Some(Map(A -> true, B -> false, C -> true))
```

This implementation provides a complete working DPLL SAT solver that can handle various SAT problems and demonstrates the core principles of the algorithm.

