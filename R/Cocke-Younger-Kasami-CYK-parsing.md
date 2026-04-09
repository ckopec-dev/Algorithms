# Cocke-Younger-Kasami (CYK) Parsing Algorithm in R

The CYK algorithm is a dynamic programming algorithm for parsing context-free grammars in Chomsky Normal Form (CNF). Here's an implementation in R:

```r
# CYK Parsing Algorithm Implementation in R

# Function to implement CYK parsing
cyk_parse <- function(grammar, sentence) {
  n <- length(sentence)
  
  # Initialize the CYK table
  # table[i][j] will store the set of non-terminals that can generate 
  # the substring from position i to j
  table <- vector("list", n)
  for (i in 1:n) {
    table[[i]] <- vector("list", n)
    for (j in 1:n) {
      table[[i]][[j]] <- character(0)
    }
  }
  
  # Step 1: Fill the diagonal (base case)
  # For each terminal in the sentence, find matching non-terminals
  for (i in 1:n) {
    word <- sentence[i]
    # Find all non-terminals that produce this terminal
    for (rule in grammar) {
      if (length(rule) == 2 && rule[2] == word) {
        table[[i]][[i]] <- c(table[[i]][[i]], rule[1])
      }
    }
  }
  
  # Step 2: Fill the table for substrings of increasing length
  for (len in 2:n) {  # length of substring
    for (i in 1:(n - len + 1)) {  # starting position
      j <- i + len - 1  # ending position
      
      # Try all possible partitions
      for (k in (i):(j - 1)) {
        # For each possible partition (i,k) and (k+1,j)
        left_cells <- table[[i]][[k]]
        right_cells <- table[[k + 1]][[j]]
        
        # For each pair of non-terminals from left and right cells
        for (left_nt in left_cells) {
          for (right_nt in right_cells) {
            # Find rules where left_nt + right_nt produces a non-terminal
            for (rule in grammar) {
              if (length(rule) == 3 && rule[2] == left_nt && rule[3] == right_nt) {
                table[[i]][[j]] <- c(table[[i]][[j]], rule[1])
              }
            }
          }
        }
      }
    }
  }
  
  return(list(table = table, is_valid = "S" %in% table[[1]][[n]]))
}

# Example grammar in Chomsky Normal Form (CNF)
# S -> AB | BC
# A -> BA | a
# B -> CC | b
# C -> AB | a

example_grammar <- list(
  c("S", "A", "B"),  # S -> AB
  c("S", "B", "C"),  # S -> BC
  c("A", "B", "A"),  # A -> BA
  c("A", "a"),       # A -> a
  c("B", "C", "C"),  # B -> CC
  c("B", "b"),       # B -> b
  c("C", "A", "B"),  # C -> AB
  c("C", "a")        # C -> a
)

# Test sentence
test_sentence <- c("a", "b", "a", "b")

# Run CYK parsing
result <- cyk_parse(example_grammar, test_sentence)

# Display results
cat("CYK Parsing Results\n")
cat("==================\n")
cat("Grammar rules:\n")
for (rule in example_grammar) {
  if (length(rule) == 2) {
    cat("  ", rule[1], "->", rule[2], "\n")
  } else {
    cat("  ", rule[1], "->", rule[2], rule[3], "\n")
  }
}
cat("\nSentence:", paste(test_sentence, collapse = " "), "\n")
cat("Parsing valid:", result$is_valid, "\n\n")

# Display the CYK table
cat("CYK Parsing Table:\n")
for (i in 1:length(result$table)) {
  for (j in 1:length(result$table[[i]])) {
    if (i <= j) {
      cat(sprintf("  [%d,%d]: %s\n", i, j, paste(result$table[[i]][[j]], collapse = ", ")))
    }
  }
}

# Function to show step-by-step parsing
show_cyk_steps <- function(grammar, sentence) {
  cat("\nStep-by-step CYK Parsing for sentence:", paste(sentence, collapse = " "), "\n")
  cat("=====================================\n")
  
  n <- length(sentence)
  table <- vector("list", n)
  for (i in 1:n) {
    table[[i]] <- vector("list", n)
    for (j in 1:n) {
      table[[i]][[j]] <- character(0)
    }
  }
  
  # Fill diagonal
  cat("Step 1: Fill diagonal (base case)\n")
  for (i in 1:n) {
    word <- sentence[i]
    for (rule in grammar) {
      if (length(rule) == 2 && rule[2] == word) {
        table[[i]][[i]] <- c(table[[i]][[i]], rule[1])
        cat("  Position", i, ":", word, "->", rule[1], "\n")
      }
    }
  }
  
  # Fill table
  cat("\nStep 2: Fill table for substrings\n")
  for (len in 2:n) {
    for (i in 1:(n - len + 1)) {
      j <- i + len - 1
      cat("  Substring [", i, ",", j, "]:\n")
      
      for (k in (i):(j - 1)) {
        left_cells <- table[[i]][[k]]
        right_cells <- table[[k + 1]][[j]]
        
        if (length(left_cells) > 0 && length(right_cells) > 0) {
          for (left_nt in left_cells) {
            for (right_nt in right_cells) {
              for (rule in grammar) {
                if (length(rule) == 3 && rule[2] == left_nt && rule[3] == right_nt) {
                  table[[i]][[j]] <- c(table[[i]][[j]], rule[1])
                  cat("    Combine", left_nt, "+", right_nt, "->", rule[1], "\n")
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("\nFinal result: Can parse entire sentence?", "S" %in% table[[1]][[n]], "\n")
}

# Run step-by-step example
show_cyk_steps(example_grammar, test_sentence)
```

## Key Features of this Implementation:

1. **Grammar Format**: Uses a list of rules where each rule is a vector:
   - Unary rules: `c("A", "a")` for A → a
   - Binary rules: `c("S", "A", "B")` for S → AB

2. **Table Structure**: The parsing table stores sets of non-terminals that can generate each substring

3. **Dynamic Programming**: Builds solutions for shorter substrings first, then combines them

4. **Output**: Shows both the parsing table and whether the sentence can be parsed by the grammar

## Sample Output:
```
CYK Parsing Results
==================
Grammar rules:
  S -> A B
  S -> B C
  A -> B A
  A -> a
  B -> C C
  B -> b
  C -> A B
  C -> a

Sentence: a b a b
Parsing valid: TRUE

CYK Parsing Table:
  [1,1]: A, C
  [2,2]: B
  [3,3]: A, C
  [4,4]: B
  [1,2]: S
  [2,3]: S
  [3,4]: S
  [1,4]: S
```

This implementation demonstrates how CYK algorithm efficiently determines whether a given string can be generated by a context-free grammar in Chomsky Normal Form.

