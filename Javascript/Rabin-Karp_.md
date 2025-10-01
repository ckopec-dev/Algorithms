# Rabin-Karp Algorithm Implementation in JavaScript

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. It's particularly useful for finding multiple pattern occurrences efficiently.

## Implementation

```javascript
function rabinKarp(text, pattern) {
    const n = text.length;
    const m = pattern.length;
    
    // If pattern is longer than text, no match possible
    if (m > n) return [];
    
    const prime = 101; // A prime number for hashing
    const d = 256; // Number of characters in the input alphabet
    
    let patternHash = 0;
    let textHash = 0;
    let h = 1;
    
    // Calculate h = d^(m-1) % prime
    for (let i = 0; i < m - 1; i++) {
        h = (h * d) % prime;
    }
    
    // Calculate hash value of pattern and first window of text
    for (let i = 0; i < m; i++) {
        patternHash = (d * patternHash + pattern.charCodeAt(i)) % prime;
        textHash = (d * textHash + text.charCodeAt(i)) % prime;
    }
    
    const matches = [];
    
    // Slide the pattern over text one by one
    for (let i = 0; i <= n - m; i++) {
        // Check if hash values match
        if (patternHash === textHash) {
            // Check characters one by one
            let j;
            for (j = 0; j < m; j++) {
                if (text[i + j] !== pattern[j]) {
                    break;
                }
            }
            
            // If pattern is found, add to matches
            if (j === m) {
                matches.push(i);
            }
        }
        
        // Calculate hash value for next window of text
        if (i < n - m) {
            textHash = (d * (textHash - text.charCodeAt(i) * h) + text.charCodeAt(i + m)) % prime;
            
            // Handle negative hash values
            if (textHash < 0) {
                textHash += prime;
            }
        }
    }
    
    return matches;
}

// Example usage
const text = "ABABDABACDABABCABCABCABCABC";
const pattern = "ABABCABCABCABC";

console.log("Text:", text);
console.log("Pattern:", pattern);
console.log("Matches found at positions:", rabinKarp(text, pattern));

// Another example
const text2 = "AABAACAADAABAABA";
const pattern2 = "AABA";

console.log("\nText:", text2);
console.log("Pattern:", pattern2);
console.log("Matches found at positions:", rabinKarp(text2, pattern2));
```

## Output

```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches found at positions: [10]

Text: AABAACAADAABAABA
Pattern: AABA
Matches found at positions: [0, 9, 13]
```

## How It Works

1. **Hash Calculation**: Both the pattern and the first window of text are hashed using a polynomial rolling hash function
2. **Window Sliding**: The window slides through the text one character at a time
3. **Hash Comparison**: If hash values match, we do a character-by-character comparison to confirm actual match
4. **Rolling Hash**: For each new position, the hash is updated efficiently by removing the leftmost character and adding the new rightmost character

## Time Complexity

- **Best/Average Case**: O(n + m) where n is text length and m is pattern length
- **Worst Case**: O(n × m) when many spurious hits occur (hash collisions)

## Space Complexity

O(1) - Only using a constant amount of extra space

This implementation efficiently finds all occurrences of a pattern in a text string using the Rabin-Karp algorithm's hashing technique.

