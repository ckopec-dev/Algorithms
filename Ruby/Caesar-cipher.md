# Caesar Cipher in Ruby

Here's an implementation of the Caesar cipher algorithm in Ruby:

```ruby
def caesar_cipher(text, shift)
  result = ""
  
  text.chars.each do |char|
    if char.match?(/[a-zA-Z]/)
      # Determine if the character is uppercase or lowercase
      if char.match?(/[A-Z]/)
        # For uppercase letters
        result += ((char.ord - 65 + shift) % 26 + 65).chr
      else
        # For lowercase letters
        result += ((char.ord - 97 + shift) % 26 + 97).chr
      end
    else
      # Non-alphabetic characters remain unchanged
      result += char
    end
  end
  
  result
end

# Example usage
message = "Hello World!"
shift_value = 3

encrypted = caesar_cipher(message, shift_value)
puts "Original: #{message}"
puts "Encrypted: #{encrypted}"

decrypted = caesar_cipher(encrypted, -shift_value)
puts "Decrypted: #{decrypted}"
```

## Output:
```
Original: Hello World!
Encrypted: Khoor Zruog!
Decrypted: Hello World!
```

## How it works:

1. **Character Processing**: The algorithm processes each character in the input text
2. **Alphabetic Check**: Only alphabetic characters are shifted; spaces and punctuation remain unchanged
3. **Case Preservation**: Uppercase and lowercase letters maintain their case
4. **Modular Arithmetic**: Uses modulo 26 to wrap around the alphabet (Z shifts to A)
5. **Bidirectional**: Can encrypt (positive shift) or decrypt (negative shift)

## Alternative Compact Version:

```ruby
def caesar_cipher_compact(text, shift)
  text.chars.map do |char|
    if char.match?(/[a-z]/)
      ((char.ord - 97 + shift) % 26 + 97).chr
    elsif char.match?(/[A-Z]/)
      ((char.ord - 65 + shift) % 26 + 65).chr
    else
      char
    end
  end.join
end
```

The Caesar cipher is a simple substitution cipher where each letter is shifted by a fixed number of positions in the alphabet.

