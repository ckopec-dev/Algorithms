# Welch's Method Algorithm in F#

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's an implementation in F#:

```fsharp
open System
open System.Numerics

/// Welch's Method for Power Spectral Density estimation
let welchMethod (signal: float[]) windowSize overlap windowType =
    let n = signal.Length
    let stepSize = int (float windowSize * (1.0 - overlap))
    let nSegments = max 1 (int (float (n - windowSize) / float stepSize) + 1)
    
    // Generate window function
    let window = 
        match windowType with
        | "hamming" -> 
            let alpha = 0.54
            let beta = 0.46
            [| for i in 0 .. windowSize - 1 -> 
                alpha - beta * cos (2.0 * Math.PI * float i / float (windowSize - 1)) |]
        | "hanning" -> 
            [| for i in 0 .. windowSize - 1 -> 
                0.5 * (1.0 - cos (2.0 * Math.PI * float i / float (windowSize - 1))) |]
        | _ -> 
            [| for i in 0 .. windowSize - 1 -> 1.0 |]
    
    // Initialize arrays
    let psd = Array.zeroCreate (windowSize / 2 + 1)
    let fftSize = windowSize
    
    // Process each segment
    for i in 0 .. nSegments - 1 do
        let startIdx = i * stepSize
        let endIdx = min (startIdx + windowSize) n
        
        // Check if we have enough data for this segment
        if endIdx - startIdx = windowSize then
            // Apply window to segment
            let segment = 
                [| for j in 0 .. windowSize - 1 -> 
                    signal.[startIdx + j] * window.[j] |]
            
            // Compute FFT
            let fftResult = fft segment
            
            // Compute power spectrum (magnitude squared)
            let power = 
                [| for j in 0 .. windowSize / 2 -> 
                    let real = fftResult.[j].Real
                    let imag = fftResult.[j].Imaginary
                    real * real + imag * imag |]
            
            // Accumulate PSD
            for j in 0 .. windowSize / 2 do
                psd.[j] <- psd.[j] + power.[j]
    
    // Average the results
    let normFactor = float nSegments
    let averagedPsd = 
        [| for i in 0 .. windowSize / 2 -> psd.[i] / normFactor |]
    
    averagedPsd

/// Simple FFT implementation (Cooley-Tukey algorithm)
let fft (x: float[]) : Complex[] =
    let n = x.Length
    if n <= 1 then
        [| Complex(x.[0], 0.0) |]
    else
        // Divide
        let even = 
            [| for i in 0 .. n/2 - 1 -> x.[2 * i] |]
        let odd = 
            [| for i in 0 .. n/2 - 1 -> x.[2 * i + 1] |]
        
        // Conquer
        let evenFft = fft even
        let oddFft = fft odd
        
        // Combine
        let result = Array.zeroCreate n
        for i in 0 .. n/2 - 1 do
            let angle = -2.0 * Math.PI * float i / float n
            let w = Complex(cos angle, sin angle)
            result.[i] <- evenFft.[i] + w * oddFft.[i]
            result.[i + n/2] <- evenFft.[i] - w * oddFft.[i]
        
        result

// Example usage
let example () =
    // Generate test signal (sum of sine waves)
    let sampleRate = 100.0
    let duration = 2.0
    let nSamples = int (sampleRate * duration)
    
    let signal = 
        [| for i in 0 .. nSamples - 1 ->
            let t = float i / sampleRate
            0.5 * sin (2.0 * Math.PI * 10.0 * t) + 
            0.3 * sin (2.0 * Math.PI * 25.0 * t) + 
            0.2 * sin (2.0 * Math.PI * 50.0 * t) + 
            0.1 * (Random().NextDouble() - 0.5) |]
    
    // Apply Welch's method
    let windowSize = 256
    let overlap = 0.5
    let windowType = "hanning"
    
    let psd = welchMethod signal windowSize overlap windowType
    
    // Print first 10 PSD values
    printfn "Welch's Method PSD estimates:"
    for i in 0 .. min 9 (psd.Length - 1) do
        printfn "Bin %d: %.4f" i psd.[i]
    
    psd

// Run example
let result = example()
```

## Key Features of this Implementation:

1. **Windowing**: Supports different window types (Hamming, Hanning, rectangular)
2. **Segmentation**: Divides signal into overlapping segments
3. **FFT Computation**: Uses Cooley-Tukey FFT algorithm
4. **Averaging**: Averages modified periodograms for better estimation
5. **Flexible Parameters**: Configurable window size, overlap, and window type

## Parameters:
- `signal`: Input signal array
- `windowSize`: Size of each segment
- `overlap`: Overlap ratio (0.0 to 1.0)
- `windowType`: Type of window function ("hamming", "hanning", or default)

## Output:
Returns an array of power spectral density estimates for each frequency bin.

This implementation provides a practical way to estimate PSD using Welch's method, which is particularly useful for noisy signals where single periodogram estimates are unreliable.

