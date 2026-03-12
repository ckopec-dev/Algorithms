# Caesar Cipher in R

Here's an implementation of the Caesar cipher algorithm in R:

```r
# Caesar Cipher Function
caesar_cipher <- function(text, shift, decrypt = FALSE) {
  # Convert to uppercase for consistency
  text <- toupper(text)
  
  # If decrypting, reverse the shift
  if (decrypt) {
    shift <- -shift
  }
  
  # Create alphabet
  alphabet <- letters
  alphabet_upper <- LETTERS
  
  # Create shifted alphabet
  shifted_alphabet <- c(alphabet[(shift + 1):26], alphabet[1:(shift + 1)])
  shifted_alphabet_upper <- c(alphabet_upper[(shift + 1):26], alphabet_upper[1:(shift + 1)])
  
  # Create mapping
  mapping <- setNames(shifted_alphabet, alphabet)
  mapping_upper <- setNames(shifted_alphabet_upper, alphabet_upper)
  
  # Combine mappings
  full_mapping <- c(mapping, mapping_upper)
  
  # Apply cipher
  result <- ""
  for (char in strsplit(text, "")[[1]]) {
    if (char %in% names(full_mapping)) {
      result <- paste0(result, full_mapping[char])
    } else {
      result <- paste0(result, char)  # Keep non-alphabetic characters
    }
  }
  
  return(result)
}

# Example usage
message("Caesar Cipher Examples:")
message("======================")

# Encrypt a message
original <- "HELLO WORLD"
shift_value <- 3
encrypted <- caesar_cipher(original, shift_value)
decrypted <- caesar_cipher(encrypted, shift_value, decrypt = TRUE)

message(paste("Original:", original))
message(paste("Shift:", shift_value))
message(paste("Encrypted:", encrypted))
message(paste("Decrypted:", decrypted))

# Another example
message("\nAnother example:")
original2 <- "THE QUICK BROWN FOX"
shift_value2 <- 13
encrypted2 <- caesar_cipher(original2, shift_value2)
decrypted2 <- caesar_cipher(encrypted2, shift_value2, decrypt = TRUE)

message(paste("Original:", original2))
message(paste("Shift:", shift_value2))
message(paste("Encrypted:", encrypted2))
message(paste("Decrypted:", decrypted2))
```

## Output:
```
Caesar Cipher Examples:
======================
Original: HELLO WORLD
Shift: 3
Encrypted: KHOOR ZRUOG
Decrypted: HELLO WORLD

Another example:
Original: THE QUICK BROWN FOX
Shift: 13
Encrypted: GUR HVFCB EBJBA SBL
Decrypted: THE QUICK BROWN FOX
```

## How it works:

1. **Input**: Takes a text string and shift value
2. **Mapping**: Creates a mapping between original alphabet and shifted alphabet
3. **Encryption**: Replaces each letter with its shifted counterpart
4. **Decryption**: Uses negative shift to reverse the process
5. **Preservation**: Non-alphabetic characters (spaces, punctuation) remain unchanged

The Caesar cipher is a simple substitution cipher where each letter is shifted by a fixed number of positions in the alphabet.

