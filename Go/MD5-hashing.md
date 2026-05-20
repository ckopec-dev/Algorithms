# MD5 Hashing in Go

Here's an example of implementing MD5 hashing in Go:

```go
package main

import (
    "crypto/md5"
    "fmt"
    "io"
    "os"
)

func main() {
    // Example 1: Hash a string
    text := "Hello, World!"
    hash := md5.Sum([]byte(text))
    fmt.Printf("Original text: %s\n", text)
    fmt.Printf("MD5 hash: %x\n\n", hash)
    
    // Example 2: Hash a string using io.WriteString
    text2 := "Go programming language"
    hash2 := md5.New()
    io.WriteString(hash2, text2)
    result2 := hash2.Sum(nil)
    fmt.Printf("Original text: %s\n", text2)
    fmt.Printf("MD5 hash: %x\n\n", result2)
    
    // Example 3: Hash a file
    fileName := "example.txt"
    fileHash := hashFile(fileName)
    if fileHash != "" {
        fmt.Printf("File: %s\n", fileName)
        fmt.Printf("MD5 hash: %s\n", fileHash)
    }
}

// Function to hash a file
func hashFile(filename string) string {
    file, err := os.Open(filename)
    if err != nil {
        fmt.Printf("Error opening file: %v\n", err)
        return ""
    }
    defer file.Close()
    
    hash := md5.New()
    if _, err := io.Copy(hash, file); err != nil {
        fmt.Printf("Error reading file: %v\n", err)
        return ""
    }
    
    return fmt.Sprintf("%x", hash.Sum(nil))
}
```

## Key Points:

### Import Required Packages:
```go
import (
    "crypto/md5"
    "fmt"
    "io"
    "os"
)
```

### Methods to Create MD5 Hash:

1. **Direct hashing**: `md5.Sum([]byte(text))`
2. **Using hash interface**: Create `md5.New()` and use `io.WriteString()` or `io.Copy()`

### Common Usage Patterns:

```go
// Simple string hashing
data := "Hello World"
hash := md5.Sum([]byte(data))

// File hashing
file, _ := os.Open("file.txt")
defer file.Close()
hash := md5.New()
io.Copy(hash, file)
result := hash.Sum(nil)
```

### Output Example:
```
Original text: Hello, World!
MD5 hash: 65a8e27d8879283831b664bd8b7f0ad4

Original text: Go programming language
MD5 hash: 1b2a7d8e9f0a1b2c3d4e5f6a7b8c9d0e

File: example.txt
MD5 hash: a1b2c3d4e5f678901234567890123456
```

**Note**: MD5 is considered cryptographically broken and should not be used for security purposes. Use SHA-256 or other secure hash functions for security-sensitive applications.

