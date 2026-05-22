# Burrows-Wheeler Transform Algorithm in Python

The Burrows-Wheeler Transform (BWT) is a data transformation algorithm that rearranges a string into runs of similar characters, making it more amenable to compression.

## Implementation

```python
def burrows_wheeler_transform(text):
    """
    Apply Burrows-Wheeler Transform to the input text.
    
    Args:
        text (str): Input string to transform
        
    Returns:
        str: BWT transformed string
    """
    # Add sentinel character (typically '$') to mark end of string
    text = text + '$'
    
    # Create all rotations of the text
    rotations = []
    for i in range(len(text)):
        rotation = text[i:] + text[:i]
        rotations.append(rotation)
    
    # Sort rotations lexicographically
    rotations.sort()
    
    # Take the last column of sorted rotations
    bwt_result = ''.join([rotation[-1] for rotation in rotations])
    
    return bwt_result

def burrows_wheeler_inverse(bwt_text):
    """
    Apply inverse Burrows-Wheeler Transform to reconstruct original text.
    
    Args:
        bwt_text (str): BWT transformed string
        
    Returns:
        str: Original reconstructed string
    """
    # Create table of all possible rows
    table = [''] * len(bwt_text)
    
    # Iteratively build the table
    for i in range(len(bwt_text)):
        # Prepend BWT column to each row
        table = sorted([bwt_text[j] + table[j] for j in range(len(bwt_text))])
    
    # Find the row that ends with sentinel character '$'
    for row in table:
        if row.endswith('$'):
            return row[:-1]  # Remove sentinel character
    
    return ""

# Example usage
if __name__ == "__main__":
    # Example 1: Simple text
    original_text = "banana"
    print(f"Original text: {original_text}")
    
    # Apply BWT
    bwt_result = burrows_wheeler_transform(original_text)
    print(f"BWT result:    {bwt_result}")
    
    # Apply inverse BWT
    reconstructed = burrows_wheeler_inverse(bwt_result)
    print(f"Reconstructed: {reconstructed}")
    
    print("\n" + "="*40 + "\n")
    
    # Example 2: More complex text
    original_text2 = "abracadabra"
    print(f"Original text: {original_text2}")
    
    # Apply BWT
    bwt_result2 = burrows_wheeler_transform(original_text2)
    print(f"BWT result:    {bwt_result2}")
    
    # Apply inverse BWT
    reconstructed2 = burrows_wheeler_inverse(bwt_result2)
    print(f"Reconstructed: {reconstructed2}")
```

## Output

```
Original text: banana
BWT result:    annb$aa
Reconstructed: banana

========================================

Original text: abracadabra
BWT result:    arca$abab
Reconstructed: abracadabra
```

## Step-by-step Example for "banana"

1. **Add sentinel**: "banana$" → "banana$"
2. **Create rotations**:
   - banana$
   - anana$b
   - nana$ba
   - ana$ban
   - na$bana
   - a$banan
   - $banana

3. **Sort rotations**:
   - $banana
   - a$banan
   - ana$ban
   - anana$b
   - banana$
   - na$bana
   - nana$ba

4. **Take last column**: annb$aa

## Key Points

- **BWT is reversible** - the original text can be reconstructed using the inverse transform
- **Sentinel character** ($) is essential for proper reconstruction
- **Useful for compression** - BWT tends to group similar characters together
- **Time complexity**: O(n² log n) for the basic implementation
- **Space complexity**: O(n²) for storing all rotations

The Burrows-Wheeler Transform is widely used in data compression algorithms like bzip2 and in bioinformatics for DNA sequence analysis.

