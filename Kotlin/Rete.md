# Rete Algorithm Example in Kotlin

Here's a simplified implementation of the Rete algorithm in Kotlin, demonstrating the core concepts of pattern matching and rule evaluation.

```kotlin
import java.util.*

// Node classes for the Rete network
sealed class ReteNode {
    abstract fun evaluate(facts: List<Fact>): List<Fact>
}

// Alpha memory node - stores facts that match a condition
class AlphaMemoryNode(val condition: (Fact) -> Boolean) : ReteNode() {
    private val memory = mutableListOf<Fact>()
    
    override fun evaluate(facts: List<Fact>): List<Fact> {
        val matchedFacts = facts.filter { condition(it) }
        memory.addAll(matchedFacts)
        return matchedFacts
    }
    
    fun getStoredFacts(): List<Fact> = memory.toList()
}

// Beta memory node - combines facts from multiple alpha memories
class BetaMemoryNode(val leftNode: ReteNode, val rightNode: ReteNode) : ReteNode() {
    private val leftMemory = mutableListOf<Fact>()
    private val rightMemory = mutableListOf<Fact>()
    
    override fun evaluate(facts: List<Fact>): List<Fact> {
        // For simplicity, we'll just return facts from left node
        // In a full implementation, this would combine memories properly
        return leftNode.evaluate(facts)
    }
    
    fun addLeftFact(fact: Fact) {
        leftMemory.add(fact)
    }
    
    fun addRightFact(fact: Fact) {
        rightMemory.add(fact)
    }
}

// Fact class representing data in the system
data class Fact(
    val id: String,
    val type: String,
    val value: Any
) {
    companion object {
        fun createPerson(id: String, name: String, age: Int): Fact {
            return Fact(id, "Person", mapOf("name" to name, "age" to age))
        }
        
        fun createOrder(id: String, customerId: String, amount: Double): Fact {
            return Fact(id, "Order", mapOf("customerId" to customerId, "amount" to amount))
        }
    }
}

// Rule class representing a business rule
data class Rule(
    val name: String,
    val conditions: List<(Fact) -> Boolean>,
    val action: (List<Fact>) -> Unit
) {
    fun matches(facts: List<Fact>): Boolean {
        return conditions.all { condition ->
            facts.any { fact -> condition(fact) }
        }
    }
    
    fun execute(facts: List<Fact>) {
        action(facts)
    }
}

// Main Rete engine class
class ReteEngine {
    private val alphaNodes = mutableListOf<AlphaMemoryNode>()
    private val rules = mutableListOf<Rule>()
    private val facts = mutableListOf<Fact>()
    
    fun addFact(fact: Fact) {
        facts.add(fact)
    }
    
    fun addRule(rule: Rule) {
        rules.add(rule)
    }
    
    fun addCondition(condition: (Fact) -> Boolean): AlphaMemoryNode {
        val node = AlphaMemoryNode(condition)
        alphaNodes.add(node)
        return node
    }
    
    fun execute() {
        println("Executing Rete network...")
        
        // Process facts through alpha nodes
        val matchedFacts = mutableListOf<Fact>()
        alphaNodes.forEach { node ->
            val results = node.evaluate(facts)
            matchedFacts.addAll(results)
        }
        
        println("Matched facts: ${matchedFacts.size}")
        matchedFacts.forEach { println("  - $it") }
        
        // Execute rules
        rules.forEach { rule ->
            if (rule.matches(matchedFacts)) {
                println("Executing rule: ${rule.name}")
                rule.execute(matchedFacts)
            }
        }
    }
    
    fun printNetwork() {
        println("Rete Network Structure:")
        println("Alpha Nodes: ${alphaNodes.size}")
        println("Rules: ${rules.size}")
        println("Facts: ${facts.size}")
    }
}

// Example usage
fun main() {
    // Create the Rete engine
    val engine = ReteEngine()
    
    // Add some sample facts
    engine.addFact(Fact.createPerson("p1", "Alice", 30))
    engine.addFact(Fact.createPerson("p2", "Bob", 25))
    engine.addFact(Fact.createOrder("o1", "p1", 150.0))
    engine.addFact(Fact.createOrder("o2", "p2", 200.0))
    
    // Add conditions (alpha nodes)
    val personCondition = engine.addCondition { fact -> fact.type == "Person" }
    val orderCondition = engine.addCondition { fact -> fact.type == "Order" }
    
    // Add rules
    val rule1 = Rule(
        name = "Find high-value customers",
        conditions = listOf({ fact -> fact.type == "Order" }),
        action = { facts ->
            val orders = facts.filter { it.type == "Order" }
            val highValueOrders = orders.filter { 
                val orderData = it.value as Map<String, Any>
                orderData["amount"] as Double > 100.0
            }
            println("High-value orders found: ${highValueOrders.size}")
            highValueOrders.forEach { order ->
                println("  - Order ${order.id}: ${order.value}")
            }
        }
    )
    
    val rule2 = Rule(
        name = "Find young people",
        conditions = listOf({ fact -> fact.type == "Person" }),
        action = { facts ->
            val people = facts.filter { it.type == "Person" }
            val youngPeople = people.filter { 
                val personData = it.value as Map<String, Any>
                personData["age"] as Int < 30
            }
            println("Young people found: ${youngPeople.size}")
            youngPeople.forEach { person ->
                println("  - ${person.value}")
            }
        }
    )
    
    engine.addRule(rule1)
    engine.addRule(rule2)
    
    // Print network structure
    engine.printNetwork()
    
    // Execute the Rete network
    engine.execute()
    
    // Show alpha memory contents
    println("\nAlpha memory contents:")
    engine.alphaNodes.forEachIndexed { index, node ->
        println("Node $index: ${node.getStoredFacts().size} facts")
    }
}
```

## Key Components Explained

### 1. **ReteNode Hierarchy**
- `AlphaMemoryNode`: Stores facts that match specific conditions
- `BetaMemoryNode`: Combines facts from multiple alpha memories

### 2. **Core Classes**
- `Fact`: Data structure representing information in the system
- `Rule`: Business rules with conditions and actions
- `ReteEngine`: Main engine that coordinates the network

### 3. **Key Features**
- **Pattern Matching**: Facts are matched against conditions
- **Memory Management**: Facts are stored and retrieved efficiently
- **Rule Execution**: Rules are executed when conditions are met
- **Network Structure**: Hierarchical organization of nodes

### 4. **Execution Flow**
1. Facts are added to the engine
2. Alpha nodes filter facts based on conditions
3. Beta nodes combine results from multiple alpha nodes
4. Rules are evaluated against matched facts
5. Appropriate actions are executed

This example demonstrates the core concepts of the Rete algorithm in a simplified form, showing how pattern matching and rule evaluation work together in a production rule system.

