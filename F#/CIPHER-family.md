# CIPHER__family Algorithm Example in F#

Here's an example of implementing a simple cipher algorithm family in F#:

```fsharp
// Define a cipher family type
type CipherFamily = 
    | Caesar of int
    | Vigenere of string
    | Substitution of char list
    | XOR of int

// Encryption function for the cipher family
let encrypt (cipher: CipherFamily) (text: string) : string =
    match cipher with
    | Caesar shift ->
        text |> Seq.map (fun c -> 
            if Char.IsLetter c then
                let baseChar = if Char.IsUpper c then 'A' else 'a'
                let shifted = (int c - int baseChar + shift) % 26
                char (int baseChar + shifted)
            else
                c)
        |> System.String.Concat
    
    | Vigenere key ->
        let keyLength = key.Length
        let mutable keyIndex = 0
        text |> Seq.map (fun c ->
            if Char.IsLetter c then
                let baseChar = if Char.IsUpper c then 'A' else 'a'
                let keyChar = key.[keyIndex % keyLength]
                let shift = int keyChar - int 'A'
                keyIndex <- keyIndex + 1
                let shifted = (int c - int baseChar + shift) % 26
                char (int baseChar + shifted)
            else
                c)
        |> System.String.Concat
    
    | Substitution substitutionMap ->
        text |> Seq.map (fun c ->
            if Char.IsLetter c then
                let index = int c - int 'A'
                if index >= 0 && index < List.length substitutionMap then
                    substitutionMap.[index]
                else
                    c
            else
                c)
        |> System.String.Concat
    
    | XOR key ->
        text |> Seq.map (fun c -> 
            char (int c ^^^ key))
        |> System.String.Concat

// Decryption function for the cipher family
let decrypt (cipher: CipherFamily) (text: string) : string =
    match cipher with
    | Caesar shift ->
        text |> Seq.map (fun c -> 
            if Char.IsLetter c then
                let baseChar = if Char.IsUpper c then 'A' else 'a'
                let shifted = (int c - int baseChar - shift + 26) % 26
                char (int baseChar + shifted)
            else
                c)
        |> System.String.Concat
    
    | Vigenere key ->
        let keyLength = key.Length
        let mutable keyIndex = 0
        text |> Seq.map (fun c ->
            if Char.IsLetter c then
                let baseChar = if Char.IsUpper c then 'A' else 'a'
                let keyChar = key.[keyIndex % keyLength]
                let shift = int keyChar - int 'A'
                keyIndex <- keyIndex + 1
                let shifted = (int c - int baseChar - shift + 26) % 26
                char (int baseChar + shifted)
            else
                c)
        |> System.String.Concat
    
    | Substitution substitutionMap ->
        // For substitution cipher, we need the reverse mapping
        let reverseMap = 
            substitutionMap 
            |> List.mapi (fun i c -> (c, char (i + int 'A')))
            |> Map.ofList
        
        text |> Seq.map (fun c ->
            if Char.IsLetter c then
                match Map.tryFind c reverseMap with
                | Some mappedChar -> mappedChar
                | None -> c
            else
                c)
        |> System.String.Concat
    
    | XOR key ->
        text |> Seq.map (fun c -> 
            char (int c ^^^ key))
        |> System.String.Concat

// Example usage
let example() =
    let message = "Hello World"
    
    // Caesar cipher with shift of 3
    let caesarCipher = Caesar 3
    let encryptedCaesar = encrypt caesarCipher message
    let decryptedCaesar = decrypt caesarCipher encryptedCaesar
    printfn "Caesar Cipher:"
    printfn "Original: %s" message
    printfn "Encrypted: %s" encryptedCaesar
    printfn "Decrypted: %s" decryptedCaesar
    printfn ""
    
    // Vigenere cipher with key "KEY"
    let vigenereCipher = Vigenere "KEY"
    let encryptedVigenere = encrypt vigenereCipher message
    let decryptedVigenere = decrypt vigenereCipher encryptedVigenere
    printfn "Vigenere Cipher:"
    printfn "Original: %s" message
    printfn "Encrypted: %s" encryptedVigenere
    printfn "Decrypted: %s" decryptedVigenere
    printfn ""
    
    // XOR cipher with key 42
    let xorCipher = XOR 42
    let encryptedXOR = encrypt xorCipher message
    let decryptedXOR = decrypt xorCipher encryptedXOR
    printfn "XOR Cipher:"
    printfn "Original: %s" message
    printfn "Encrypted: %s" encryptedXOR
    printfn "Decrypted: %s" decryptedXOR

// Run the example
example()
```

## Key Features of This Implementation:

1. **Algebraic Data Types**: Uses discriminated unions to represent different cipher types
2. **Pattern Matching**: Each cipher type is handled differently through pattern matching
3. **Generic Cipher Family**: Supports multiple cipher algorithms within the same type system
4. **Extensible Design**: Easy to add new cipher types by extending the union
5. **Pure Functions**: Encryption and decryption functions are pure with no side effects
6. **Type Safety**: Compile-time checking ensures correct cipher usage

This demonstrates how F#'s type system and functional programming features can be used to create a flexible cipher algorithm family.

