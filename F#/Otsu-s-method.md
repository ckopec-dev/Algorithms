# Otsu's Method Implementation in F#

Otsu's method is a popular algorithm for automatic threshold selection in image processing. Here's a complete implementation in F#:

```fsharp
open System
open System.Drawing
open System.Drawing.Imaging

// Otsu's method implementation
let otsuThreshold (histogram: int[]) : int =
    let totalPixels = histogram |> Array.sum
    
    let mutable maxVariance = 0.0
    let mutable bestThreshold = 0
    let mutable w1 = 0.0
    let mutable w2 = 0.0
    let mutable sum1 = 0.0
    let mutable sum2 = 0.0
    let mutable mean1 = 0.0
    let mutable mean2 = 0.0
    
    for threshold in 0..255 do
        w1 <- (histogram.[..threshold] |> Array.sum |> float) / totalPixels
        w2 <- (histogram.[(threshold + 1)..255] |> Array.sum |> float) / totalPixels
        
        // Calculate means
        sum1 <- 
            [0..threshold]
            |> List.sumBy (fun i -> float histogram.[i] * float i)
        
        sum2 <- 
            [(threshold + 1)..255]
            |> List.sumBy (fun i -> float histogram.[i] * float i)
        
        mean1 <- if w1 > 0.0 then sum1 / (w1 * totalPixels) else 0.0
        mean2 <- if w2 > 0.0 then sum2 / (w2 * totalPixels) else 0.0
        
        // Calculate between-class variance
        let betweenClassVariance = w1 * w2 * (mean1 - mean2) ** 2.0
        
        if betweenClassVariance > maxVariance then
            maxVariance <- betweenClassVariance
            bestThreshold <- threshold
    
    bestThreshold

// Helper function to calculate histogram from grayscale image
let calculateHistogram (bitmap: Bitmap) : int[] =
    let histogram = Array.create 256 0
    
    for y in 0 .. bitmap.Height - 1 do
        for x in 0 .. bitmap.Width - 1 do
            let pixel = bitmap.GetPixel(x, y)
            let grayValue = int (0.299 * float pixel.R + 0.587 * float pixel.G + 0.114 * float pixel.B)
            histogram.[grayValue] <- histogram.[grayValue] + 1
    
    histogram

// Apply threshold to image
let applyThreshold (bitmap: Bitmap) (threshold: int) : Bitmap =
    let result = new Bitmap(bitmap.Width, bitmap.Height)
    
    for y in 0 .. bitmap.Height - 1 do
        for x in 0 .. bitmap.Width - 1 do
            let pixel = bitmap.GetPixel(x, y)
            let grayValue = int (0.299 * float pixel.R + 0.587 * float pixel.G + 0.114 * float pixel.B)
            let newColor = if grayValue > threshold then Color.White else Color.Black
            result.SetPixel(x, y, newColor)
    
    result

// Complete example usage
let exampleOtsuMethod () =
    try
        // Load an image (you would replace this with your image path)
        let originalImage = new Bitmap("sample_image.jpg")
        
        // Calculate histogram
        let histogram = calculateHistogram originalImage
        
        // Apply Otsu's method to find optimal threshold
        let threshold = otsuThreshold histogram
        
        printfn "Optimal threshold: %d" threshold
        
        // Apply threshold to create binary image
        let binaryImage = applyThreshold originalImage threshold
        
        // Save the result
        binaryImage.Save("binary_output.png", ImageFormat.Png)
        
        printfn "Binary image saved successfully!"
        
        // Clean up
        originalImage.Dispose()
        binaryImage.Dispose()
        
    with
    | ex -> printfn "Error: %s" ex.Message

// Alternative implementation using a more functional approach
let otsuThresholdFunctional (histogram: int[]) : int =
    let totalPixels = float (Array.sum histogram)
    
    histogram
    |> Array.mapi (fun i count ->
        let w1 = float (Array.sumBy (fun x -> if x <= i then x else 0) histogram) / totalPixels
        let w2 = float (Array.sumBy (fun x -> if x > i then x else 0) histogram) / totalPixels
        
        let mean1 = 
            if w1 > 0.0 then 
                float (Array.sumBy (fun x -> if x <= i then x * x else 0) histogram) / (w1 * totalPixels)
            else 0.0
            
        let mean2 = 
            if w2 > 0.0 then 
                float (Array.sumBy (fun x -> if x > i then x * x else 0) histogram) / (w2 * totalPixels)
            else 0.0
        
        let betweenClassVariance = w1 * w2 * (mean1 - mean2) ** 2.0
        (i, betweenClassVariance))
    |> Array.maxBy snd
    |> fst

// Simple test example
let simpleTest () =
    // Create a simple histogram for testing
    let testHistogram = 
        [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 0-9: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 10-19: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 20-29: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 30-39: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 40-49: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 50-59: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 60-69: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 70-79: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 80-89: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 90-99: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 100-109: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 110-119: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 120-129: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 130-139: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 140-149: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 150-159: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 160-169: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 170-179: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 180-189: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 190-199: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 200-209: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 210-219: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 220-229: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 230-239: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 240-249: 0 pixels
          0; 0; 0; 0; 0; 0; 0; 0; 0; 0;  // 250-255: 0 pixels
        |]
    
    let result = otsuThreshold testHistogram
    printfn "Otsu threshold result: %d" result

// Run the example
simpleTest()
```

## Key Features of This Implementation:

1. **Histogram Calculation**: Computes the grayscale histogram of an image
2. **Otsu's Algorithm**: Implements the mathematical approach to find optimal threshold
3. **Binary Image Creation**: Applies the threshold to create a binary image
4. **Functional Approach**: Includes both imperative and functional implementations
5. **Error Handling**: Basic error handling for file operations

## How Otsu's Method Works:

1. **Calculate histogram** of grayscale pixel values
2. **Iterate through all possible thresholds** (0-255)
3. **For each threshold**, calculate:
   - Within-class variance
   - Between-class variance
4. **Select threshold** that maximizes between-class variance

## Usage Notes:

- This implementation assumes grayscale images
- Requires System.Drawing for image processing
- The algorithm works best on images with bimodal histograms
- For real applications, consider using optimized libraries like ImageSharp or OpenCV for better performance

The method automatically determines the optimal threshold value that best separates foreground from background in an image.

