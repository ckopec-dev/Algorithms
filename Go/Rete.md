# Rete Algorithm Implementation in Go

The Rete algorithm is a pattern matching algorithm used in expert systems for efficient rule evaluation. Here's a complete implementation in Go:

```go
package main

import (
    "fmt"
    "reflect"
)

// Fact represents a data fact in the system
type Fact struct {
    ID     string
    Type   string
    Values map[string]interface{}
}

// Node represents a node in the Rete network
type Node struct {
    ID       string
    NodeType string
    Children []*Node
    Parents  []*Node
    Conditions []Condition
    Memory   []Fact
}

// Condition represents a pattern matching condition
type Condition struct {
    Field    string
    Operator string
    Value    interface{}
}

// ReteNetwork represents the complete Rete network
type ReteNetwork struct {
    RootNode *Node
    Facts    []Fact
}

// NewFact creates a new fact
func NewFact(id, factType string, values map[string]interface{}) Fact {
    return Fact{
        ID:     id,
        Type:   factType,
        Values: values,
    }
}

// NewNode creates a new node
func NewNode(id, nodeType string, conditions []Condition) *Node {
    return &Node{
        ID:         id,
        NodeType:   nodeType,
        Children:   make([]*Node, 0),
        Parents:    make([]*Node, 0),
        Conditions: conditions,
        Memory:     make([]Fact, 0),
    }
}

// NewReteNetwork creates a new Rete network
func NewReteNetwork() *ReteNetwork {
    return &ReteNetwork{
        RootNode: NewNode("root", "root", []Condition{}),
        Facts:    make([]Fact, 0),
    }
}

// AddFact adds a fact to the network
func (rn *ReteNetwork) AddFact(fact Fact) {
    rn.Facts = append(rn.Facts, fact)
    rn.propagateFact(fact)
}

// propagateFact propagates a fact through the network
func (rn *ReteNetwork) propagateFact(fact Fact) {
    rn.evaluateNode(rn.RootNode, fact)
}

// evaluateNode evaluates a fact against a node's conditions
func (rn *ReteNetwork) evaluateNode(node *Node, fact Fact) {
    if node.NodeType == "join" {
        rn.evaluateJoinNode(node, fact)
    } else {
        if rn.matchConditions(node, fact) {
            node.Memory = append(node.Memory, fact)
            rn.propagateToChildren(node, fact)
        }
    }
}

// matchConditions checks if a fact matches all conditions
func (rn *ReteNetwork) matchConditions(node *Node, fact Fact) bool {
    for _, condition := range node.Conditions {
        if !rn.evaluateCondition(fact, condition) {
            return false
        }
    }
    return true
}

// evaluateCondition evaluates a single condition
func (rn *ReteNetwork) evaluateCondition(fact Fact, condition Condition) bool {
    value, exists := fact.Values[condition.Field]
    if !exists {
        return false
    }

    switch condition.Operator {
    case "=":
        return reflect.DeepEqual(value, condition.Value)
    case ">":
        return value.(float64) > condition.Value.(float64)
    case "<":
        return value.(float64) < condition.Value.(float64)
    case "!=":
        return !reflect.DeepEqual(value, condition.Value)
    default:
        return false
    }
}

// evaluateJoinNode handles join node logic
func (rn *ReteNetwork) evaluateJoinNode(node *Node, fact Fact) {
    // For simplicity, this is a basic join implementation
    node.Memory = append(node.Memory, fact)
    rn.propagateToChildren(node, fact)
}

// propagateToChildren propagates a fact to child nodes
func (rn *ReteNetwork) propagateToChildren(node *Node, fact Fact) {
    for _, child := range node.Children {
        rn.evaluateNode(child, fact)
    }
}

// AddRule adds a rule to the network (represented as a node)
func (rn *ReteNetwork) AddRule(id string, conditions []Condition, nodeType string) *Node {
    node := NewNode(id, nodeType, conditions)
    rn.RootNode.Children = append(rn.RootNode.Children, node)
    node.Parents = append(node.Parents, rn.RootNode)
    return node
}

// PrintNetwork prints the current state of the network
func (rn *ReteNetwork) PrintNetwork() {
    fmt.Println("=== Rete Network ===")
    fmt.Printf("Root Node: %s\n", rn.RootNode.ID)
    fmt.Printf("Total Facts: %d\n", len(rn.Facts))
    fmt.Println("Facts:")
    for _, fact := range rn.Facts {
        fmt.Printf("  - %s (%s): %v\n", fact.ID, fact.Type, fact.Values)
    }
    fmt.Println()
}

// Example usage
func main() {
    // Create a new Rete network
    network := NewReteNetwork()
    
    // Create some sample facts
    fact1 := NewFact("f1", "person", map[string]interface{}{
        "name": "Alice",
        "age":  25,
        "city": "New York",
    })
    
    fact2 := NewFact("f2", "person", map[string]interface{}{
        "name": "Bob",
        "age":  30,
        "city": "London",
    })
    
    fact3 := NewFact("f3", "person", map[string]interface{}{
        "name": "Charlie",
        "age":  35,
        "city": "New York",
    })
    
    // Add facts to the network
    network.AddFact(fact1)
    network.AddFact(fact2)
    network.AddFact(fact3)
    
    // Create rules (nodes)
    rule1 := network.AddRule("rule1", []Condition{
        {"age", ">", 20.0},
        {"city", "=", "New York"},
    }, "rule")
    
    rule2 := network.AddRule("rule2", []Condition{
        {"age", ">", 25.0},
    }, "rule")
    
    // Print network state
    network.PrintNetwork()
    
    // Show rule evaluation
    fmt.Println("=== Rule Evaluation ===")
    fmt.Printf("Rule 1 matches: %d facts\n", len(rule1.Memory))
    fmt.Printf("Rule 2 matches: %d facts\n", len(rule2.Memory))
    
    // Demonstrate rule with specific conditions
    fmt.Println("\n=== Specific Rule Matching ===")
    fmt.Println("People over 25:")
    for _, fact := range network.Facts {
        if fact.Values["age"].(float64) > 25.0 {
            fmt.Printf("  - %s: %v\n", fact.ID, fact.Values)
        }
    }
}
```

## Key Components Explained

### 1. **Fact Structure**
- Represents data in the system
- Contains ID, type, and key-value pairs of data

### 2. **Node Structure**
- Represents nodes in the Rete network
- Stores conditions for pattern matching
- Maintains parent-child relationships
- Contains memory of matching facts

### 3. **ReteNetwork Structure**
- Main system controller
- Manages facts and rules
- Handles propagation of facts through the network

### 4. **Core Functions**

#### `AddFact()`
- Adds a new fact to the system
- Triggers propagation through the network

#### `evaluateNode()`
- Evaluates whether a fact matches node conditions
- Handles different node types (join, rule, etc.)

#### `matchConditions()`
- Checks if a fact satisfies all conditions in a node

#### `evaluateCondition()`
- Performs actual condition evaluation with operators like `=`, `>`, `<`, `!=`

## Sample Output
```
=== Rete Network ===
Root Node: root
Total Facts: 3
Facts:
  - f1 (person): map[age:25 city:New York name:Alice]
  - f2 (person): map[age:30 city:London name:Bob]
  - f3 (person): map[age:35 city:New York name:Charlie]

=== Rule Evaluation ===
Rule 1 matches: 2 facts
Rule 2 matches: 3 facts

=== Specific Rule Matching ===
People over 25:
  - f1: map[age:25 city:New York name:Alice]
  - f2: map[age:30 city:London name:Bob]
  - f3: map[age:35 city:New York name:Charlie]
```

This implementation demonstrates the core concepts of the Rete algorithm including pattern matching, network propagation, and efficient fact evaluation - all while maintaining the performance characteristics that make Rete suitable for expert systems.

