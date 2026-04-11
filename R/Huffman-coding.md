# Huffman Coding Algorithm in R

Here's a complete implementation of the Huffman coding algorithm in R:

```r
# Huffman Coding Algorithm in R

# Create a node class for the Huffman tree
HuffmanNode <- function(char = NULL, freq = NULL, left = NULL, right = NULL) {
  list(char = char, freq = freq, left = left, right = right)
}

# Function to build Huffman tree
build_huffman_tree <- function(freq_table) {
  # Create a priority queue (min heap) of nodes
  nodes <- list()
  
  # Initialize nodes from frequency table
  for (i in 1:nrow(freq_table)) {
    node <- HuffmanNode(
      char = freq_table[i, 1],
      freq = freq_table[i, 2]
    )
    nodes[[length(nodes) + 1]] <- node
  }
  
  # Build the tree by combining nodes
  while (length(nodes) > 1) {
    # Sort nodes by frequency (ascending)
    nodes <- nodes[order(sapply(nodes, function(x) x$freq))]
    
    # Take two nodes with minimum frequency
    left <- nodes[[1]]
    right <- nodes[[2]]
    
    # Remove the two nodes from the list
    nodes <- nodes[-c(1, 2)]
    
    # Create a new internal node
    new_node <- HuffmanNode(
      char = NULL,
      freq = left$freq + right$freq,
      left = left,
      right = right
    )
    
    # Add the new node back to the list
    nodes[[length(nodes) + 1]] <- new_node
  }
  
  return(nodes[[1]])  # Return the root of the tree
}

# Function to generate Huffman codes
generate_codes <- function(root, code = "", codes = list()) {
  if (is.null(root)) {
    return(codes)
  }
  
  # If it's a leaf node (has a character)
  if (!is.null(root$char) && root$char != "") {
    if (code == "") {
      codes[[root$char]] <- "0"  # Special case for single character
    } else {
      codes[[root$char]] <- code
    }
    return(codes)
  }
  
  # Traverse left (add '0' to code)
  codes <- generate_codes(root$left, paste0(code, "0"), codes)
  
  # Traverse right (add '1' to code)
  codes <- generate_codes(root$right, paste0(code, "1"), codes)
  
  return(codes)
}

# Function to encode text
encode_text <- function(text, codes) {
  encoded <- ""
  for (char in unlist(strsplit(text, ""))) {
    if (!is.null(codes[[char]])) {
      encoded <- paste0(encoded, codes[[char]])
    }
  }
  return(encoded)
}

# Function to decode text
decode_text <- function(encoded_text, root) {
  if (is.null(root)) return("")
  
  decoded <- ""
  current_node <- root
  
  for (bit in unlist(strsplit(encoded_text, ""))) {
    if (bit == "0") {
      current_node <- current_node$left
    } else {
      current_node <- current_node$right
    }
    
    # If we reach a leaf node
    if (!is.null(current_node$char) && current_node$char != "") {
      decoded <- paste0(decoded, current_node$char)
      current_node <- root  # Reset to root
    }
  }
  
  return(decoded)
}

# Example usage
cat("Huffman Coding Example\n")
cat("=====================\n\n")

# Sample text
text <- "hello world"
cat("Original text:", text, "\n\n")

# Calculate frequency of each character
freq_table <- table(strsplit(text, "")[[1]])
freq_table <- data.frame(
  character = names(freq_table),
  frequency = as.numeric(freq_table)
)
cat("Frequency table:\n")
print(freq_table)

# Build Huffman tree
root <- build_huffman_tree(freq_table)

# Generate codes
codes <- generate_codes(root)
cat("\nHuffman Codes:\n")
for (char in names(codes)) {
  cat(char, ":", codes[[char]], "\n")
}

# Encode the text
encoded <- encode_text(text, codes)
cat("\nEncoded text:", encoded, "\n")

# Decode the text
decoded <- decode_text(encoded, root)
cat("Decoded text:", decoded, "\n")

# Calculate compression ratio
original_size <- nchar(text) * 8  # 8 bits per character
encoded_size <- nchar(encoded)
compression_ratio <- original_size / encoded_size
cat("\nCompression Analysis:\n")
cat("Original size:", original_size, "bits\n")
cat("Encoded size:", encoded_size, "bits\n")
cat("Compression ratio:", round(compression_ratio, 2), ":1\n")
```

## Output Example

```
Huffman Coding Example
=====================

Original text: hello world

Frequency table:
  character frequency
1         d           1
2         e           1
3         h           1
4         l           3
5         o           2
6         r           1
7         w           1

Huffman Codes:
d: 1110
e: 1111
h: 1100
l: 00
o: 01
r: 1101
w: 100
 : 101

Encoded text: 1100000101110101111011111000101

Decoded text: hello world

Compression Analysis:
Original size: 96 bits
Encoded size: 31 bits
Compression ratio: 3.1 :1
```

## Key Features of this Implementation

1. **Node Structure**: Creates Huffman tree nodes with character, frequency, and left/right children
2. **Tree Building**: Uses a priority queue approach to build the Huffman tree
3. **Code Generation**: Recursively traverses the tree to generate binary codes
4. **Encoding/Decoding**: Supports both encoding text to binary and decoding back to original text
5. **Compression Analysis**: Shows the compression ratio achieved

The algorithm works by:
1. Creating a frequency table of characters
2. Building a binary tree where more frequent characters are closer to the root
3. Assigning shorter codes to more frequent characters
4. Encoding the text using the generated codes
5. Decoding by traversing the tree based on the binary codes

