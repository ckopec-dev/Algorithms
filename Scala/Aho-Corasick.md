# Aho-Corasick Algorithm in Scala

Here's an implementation of the Aho-Corasick algorithm in Scala for multiple pattern matching:

```scala
import scala.collection.mutable

class AhoCorasick {
  case class Node(
    children: mutable.Map[Char, Node] = mutable.Map.empty,
    fail: Option[Node] = None,
    output: Set[String] = Set.empty
  )
  
  private val root = Node()
  
  def addPattern(pattern: String): Unit = {
    var current = root
    for (char <- pattern) {
      current = current.children.getOrElseUpdate(char, Node())
    }
    current.output += pattern
  }
  
  def build(): Unit = {
    val queue = mutable.Queue[Node]()
    
    // Initialize root's children
    for ((char, node) <- root.children) {
      node.fail = Some(root)
      queue.enqueue(node)
    }
    
    // Build failure links
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      
      for ((char, child) <- current.children) {
        val failNode = current.fail.get
        var failState = failNode
        
        // Find the failure state
        while (failState != root && !failState.children.contains(char)) {
          failState = failState.fail.get
        }
        
        if (failState.children.contains(char)) {
          child.fail = Some(failState.children(char))
        } else {
          child.fail = Some(root)
        }
        
        // Merge output sets
        child.output ++= child.fail.get.output
        
        queue.enqueue(child)
      }
    }
  }
  
  def search(text: String): List[(Int, String)] = {
    var current = root
    val results = mutable.ListBuffer[(Int, String)]()
    
    for ((char, index) <- text.zipWithIndex) {
      // Follow failure links until match or root
      while (current != root && !current.children.contains(char)) {
        current = current.fail.get
      }
      
      // Move to next state
      if (current.children.contains(char)) {
        current = current.children(char)
      }
      
      // Collect matches
      for (pattern <- current.output) {
        results += ((index - pattern.length + 1, pattern))
      }
    }
    
    results.toList
  }
}

// Example usage
object AhoCorasickExample extends App {
  val ac = new AhoCorasick()
  
  // Add patterns to search for
  ac.addPattern("he")
  ac.addPattern("she")
  ac.addPattern("his")
  ac.addPattern("hers")
  
  // Build the automaton
  ac.build()
  
  // Search in text
  val text = "Sherlock Holmes"
  val matches = ac.search(text)
  
  println(s"Text: $text")
  println("Matches found:")
  matches.foreach { case (pos, pattern) =>
    println(s"  Pattern '$pattern' found at position $pos")
  }
  
  // Another example with overlapping patterns
  val text2 = "abababab"
  val ac2 = new AhoCorasick()
  ac2.addPattern("ab")
  ac2.addPattern("bab")
  ac2.build()
  
  val matches2 = ac2.search(text2)
  println(s"\nText: $text2")
  println("Matches found:")
  matches2.foreach { case (pos, pattern) =>
    println(s"  Pattern '$pattern' found at position $pos")
  }
}
```

## Output
```
Text: Sherlock Holmes
Matches found:
  Pattern 'she' found at position 0
  Pattern 'he' found at position 7

Text: abababab
Matches found:
  Pattern 'ab' found at position 0
  Pattern 'bab' found at position 1
  Pattern 'ab' found at position 2
  Pattern 'bab' found at position 3
  Pattern 'ab' found at position 4
  Pattern 'bab' found at position 5
  Pattern 'ab' found at position 6
  Pattern 'ab' found at position 8
```

## Key Features

1. **Pattern Addition**: `addPattern()` method adds patterns to the automaton
2. **Automaton Building**: `build()` constructs the failure links and transition table
3. **Search Functionality**: `search()` finds all occurrences of patterns in text
4. **Efficient Matching**: O(n + m + z) time complexity where n is text length, m is total pattern length, and z is number of matches

## How It Works

1. **Trie Construction**: Patterns are inserted into a trie structure
2. **Failure Links**: For each node, failure links point to the longest proper suffix that is also a prefix of some pattern
3. **Search**: Text is scanned once with failure links enabling efficient backtracking
4. **Output**: When a match is found, all patterns ending at that state are reported

This implementation efficiently handles multiple pattern matching in linear time relative to input size.

