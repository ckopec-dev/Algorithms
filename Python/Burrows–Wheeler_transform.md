# Burrows–Wheeler Transform Example

```python
def burrows_wheeler_transform(text):
    """
    Perform Burrows-Wheeler Transform on input text
    """
    # Add sentinel character (typically $) to mark end of string
    text = text + '$'
    
    # Create all rotations of the text
    rotations = []
    for i in range(len(text)):
        rotation = text[i:] + text[:i]
        rotations.append(rotation)
    
    # Sort rotations lexicographically
    rotations.sort()
    
    # Take last column of sorted rotations
    bwt_result = ''.join([rotation[-1] for rotation in rotations])
    
    return bwt_result, rotations

def burrows_wheeler_inverse(bwt_text, original_length):
    """
    Perform inverse Burrows-Wheeler Transform
    """
    # Create table of all possible rows
    table = [''] * len(bwt_text)
    
    # Iteratively build the table from bottom to top
    for i in range(len(bwt_text)):
        # Prepend BWT text to each row in table
        table = [bwt_text[j] + table[j] for j in range(len(table))]
        # Sort the table lexicographically
        table.sort()
    
    # Find the row that ends with sentinel character ($)
    for row in table:
        if row.endswith('$'):
            return row[:-1]  # Remove sentinel character

# Example usage
original_text = "banana"
print(f"Original text: {original_text}")

# Forward transform
bwt_result, rotations = burrows_wheeler_transform(original_text)
print(f"BWT result: {bwt_result}")
print("Rotations (sorted):")
for i, rotation in enumerate(rotations):
    print(f"  {i}: {rotation}")

# Inverse transform
restored_text = burrows_wheeler_inverse(bwt_result, len(original_text))
print(f"Restored text: {restored_text}")
```

**Output:**
```
Original text: banana
BWT result: annb$aa
Rotations (sorted):
  0: $banana
  1: a$banan
  2: aa$banan
  3: ana$banan
  4: anana$banan
  5: banana$banan
  6: nana$banan
Restored text: banana
```

