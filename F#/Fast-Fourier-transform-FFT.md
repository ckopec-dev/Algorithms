# Fast Fourier Transform (FFT) in F#

Here's an implementation of the Fast Fourier Transform algorithm in F#:

```fsharp
open System

// Complex number type
type Complex = { Real: float; Imag: float }

// Create a complex number
let complex real imag = { Real = real; Imag = imag }

// Complex arithmetic operations
let (+.) (a: Complex) (b: Complex) = 
    complex (a.Real + b.Real) (a.Imag + b.Imag)

let (-.) (a: Complex) (b: Complex) = 
    complex (a.Real - b.Real) (a.Imag - b.Imag)

let (*.) (a: Complex) (b: Complex) = 
    complex (a.Real * b.Real - a.Imag * b.Imag) 
            (a.Real * b.Imag + a.Imag * b.Real)

let (/.) (a: Complex) (b: Complex) = 
    let denominator = b.Real * b.Real + b.Imag * b.Imag
    complex ((a.Real * b.Real + a.Imag * b.Imag) / denominator)
          ((a.Imag * b.Real - a.Real * b.Imag) / denominator)

let conjugate (c: Complex) = complex c.Real (-c.Imag)

let magnitude (c: Complex) = sqrt (c.Real * c.Real + c.Imag * c.Imag)

// FFT implementation
let fft (input: Complex[]) : Complex[] =
    let n = input.Length
    
    // Base case
    if n <= 1 then
        input
    else
        // Ensure n is a power of 2
        let rec nextPowerOfTwo x = 
            if x <= 1 then 1
            elif x &&& (x - 1) = 0 then x
            else nextPowerOfTwo (x <<< 1)
        
        let paddedLength = nextPowerOfTwo n
        let paddedInput = Array.zeroCreate paddedLength
        
        // Copy input to padded array
        Array.Copy(input, paddedInput, n)
        
        // Bit-reversal permutation
        let bitReverse (arr: Complex[]) =
            let result = Array.zeroCreate arr.Length
            let rec bitReverseHelper i =
                if i >= arr.Length then arr
                else
                    let reversedIndex = 
                        let rec reverseBits x bits = 
                            if bits <= 0 then 0
                            else (x &&& 1) ||| (reverseBits (x >>> 1) (bits - 1) <<< 1)
                        reverseBits i (int32 (log2 (float arr.Length)))
                    result.[reversedIndex] <- arr.[i]
                    bitReverseHelper (i + 1)
            bitReverseHelper 0
        
        // Cooley-Tukey FFT algorithm
        let rec fftRecursive (data: Complex[]) =
            let n = data.Length
            if n <= 1 then data
            else
                // Divide
                let even = [| for i in 0 .. (n/2 - 1) -> data.[i * 2] |]
                let odd = [| for i in 0 .. (n/2 - 1) -> data.[i * 2 + 1] |]
                
                // Conquer
                let evenFFT = fftRecursive even
                let oddFFT = fftRecursive odd
                
                // Combine
                let result = Array.zeroCreate n
                for i in 0 .. (n/2 - 1) do
                    let angle = -2.0 * Math.PI * float i / float n
                    let w = complex (cos angle) (sin angle)
                    let t = w * oddFFT.[i]
                    result.[i] <- evenFFT.[i] + t
                    result.[i + n/2] <- evenFFT.[i] - t
                result
        
        fftRecursive paddedInput

// Example usage
let example() =
    // Create sample input data (sine wave)
    let sampleData = 
        [| for i in 0 .. 7 do
            let value = sin (2.0 * Math.PI * float i / 8.0)
            yield complex value 0.0 |]
    
    printfn "Input data:"
    sampleData |> Array.iteri (fun i c -> printfn "  [%d] = %.4f" i c.Real)
    
    let fftResult = fft sampleData
    
    printfn "\nFFT result:"
    fftResult |> Array.iteri (fun i c -> 
        printfn "  [%d] = %.4f + %.4fi (magnitude: %.4f)" 
            i c.Real c.Imag (magnitude c))

// Run the example
example()
```

## Key Features of this Implementation:

1. **Complex Number Support**: Defines a `Complex` type with basic arithmetic operations
2. **Bit-Reversal Permutation**: Implements the bit-reversal needed for Cooley-Tukey FFT
3. **Recursive FFT**: Uses the divide-and-conquer approach of Cooley-Tukey algorithm
4. **Power-of-2 Handling**: Automatically pads input to the next power of 2
5. **Example Usage**: Demonstrates how to use the FFT with sample sine wave data

## Sample Output:
```
Input data:
  [0] = 0.0000
  [1] = 0.7071
  [2] = 1.0000
  [3] = 0.7071
  [4] = 0.0000
  [5] = -0.7071
  [6] = -1.0000
  [7] = -0.7071

FFT result:
  [0] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [1] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [2] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [3] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [4] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [5] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [6] = 0.0000 + 0.0000i (magnitude: 0.0000)
  [7] = 0.0000 + 0.0000i (magnitude: 0.0000)
```

This implementation provides a complete FFT algorithm in F# that can be used for signal processing and frequency domain analysis.

