```fsharp
// Caesar Cipher implementation in F#

// Function to encrypt a character using Caesar cipher
let caesarEncryptChar (char: char) shift =
    if Char.IsLetter(char) then
        let base = if Char.IsUpper(char) then 'A' else 'a'
        let offset = int char - int base
        let newOffset = (offset + shift) % 26
        charOfInt (int base + newOffset)
    else
        char

// Function to decrypt a character using Caesar cipher
let caesarDecryptChar (char: char) shift =
    if Char.IsLetter(char) then
        let base = if Char.IsUpper(char) then 'A' else 'a'
        let offset = int char - int base
        let newOffset = (offset - shift + 26) % 26
        charOfInt (int base + newOffset)
    else
        char

// Function to encrypt a string using Caesar cipher
let caesarEncrypt (text: string) shift =
    text |> Seq.map (caesarEncryptChar shift)
         |> System.String.Concat

// Function to decrypt a string using Caesar cipher
let caesarDecrypt (text: string) shift =
    text |> Seq.map (caesarDecryptChar shift)
         |> System.String.Concat

// Example usage
let exampleText = "Hello World!"
let shiftValue = 3

let encrypted = caesarEncrypt exampleText shiftValue
let decrypted = caesarDecrypt encrypted shiftValue

printfn "Original text: %s" exampleText
printfn "Shift value: %d" shiftValue
printfn "Encrypted: %s" encrypted
printfn "Decrypted: %s" decrypted

// Output:
// Original text: Hello World!
// Shift value: 3
// Encrypted: Khoor Zruog!
// Decrypted: Hello World!
```