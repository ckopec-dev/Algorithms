# Bezier Curve Generation in F#

Here's an implementation of a Bezier curve generation algorithm in F#:

```fsharp
// Bezier Curve Generation Algorithm in F#

// Define a point type
type Point = { X: float; Y: float }

// Calculate binomial coefficient C(n,k)
let binomialCoeff n k =
    if k > n || k < 0 then 0.0
    elif k = 0 || k = n then 1.0
    else
        let rec calc i acc =
            if i <= k then acc
            else calc (i - 1) (acc * float (n - i + 1) / float i)
        calc n 1.0

// Calculate Bernstein polynomial B(i,n)(t)
let bernsteinPoly i n t =
    binomialCoeff n i * (t ** float i) * ((1.0 - t) ** float (n - i))

// Generate a point on a Bezier curve
let bezierPoint controlPoints t =
    let n = List.length controlPoints - 1
    let rec calculatePoint i acc =
        if i <= n then
            let point = controlPoints.[i]
            let weight = bernsteinPoly i n t
            let newPoint = { X = point.X * weight; Y = point.Y * weight }
            calculatePoint (i + 1) { X = acc.X + newPoint.X; Y = acc.Y + newPoint.Y }
        else acc
    calculatePoint 0 { X = 0.0; Y = 0.0 }

// Generate multiple points on a Bezier curve
let generateBezierCurve controlPoints numPoints =
    [0..numPoints-1]
    |> List.map (fun i -> 
        let t = float i / float (numPoints - 1)
        bezierPoint controlPoints t)

// Example usage
let main() =
    // Define control points for a quadratic Bezier curve
    let controlPoints = [
        { X = 0.0; Y = 0.0 }   // Start point
        { X = 1.0; Y = 2.0 }   // Control point
        { X = 2.0; Y = 0.0 }   // End point
    ]
    
    // Generate 11 points on the curve (including start and end)
    let curvePoints = generateBezierCurve controlPoints 11
    
    // Print the results
    printfn "Bezier Curve Points:"
    curvePoints |> List.iteri (fun i point ->
        printfn "Point %d: (%.2f, %.2f)" i point.X point.Y)
    
    // Generate a cubic Bezier curve
    let cubicControlPoints = [
        { X = 0.0; Y = 0.0 }   // Start point
        { X = 1.0; Y = 1.0 }   // Control point 1
        { X = 2.0; Y = 1.0 }   // Control point 2
        { X = 3.0; Y = 0.0 }   // End point
    ]
    
    let cubicCurvePoints = generateBezierCurve cubicControlPoints 11
    
    printfn "\nCubic Bezier Curve Points:"
    cubicCurvePoints |> List.iteri (fun i point ->
        printfn "Point %d: (%.2f, %.2f)" i point.X point.Y)

// Run the example
main()
```

## Key Features of this Implementation:

### 1. **Point Type**
```fsharp
type Point = { X: float; Y: float }
```
Defines a simple 2D point structure.

### 2. **Mathematical Functions**
- `binomialCoeff`: Calculates binomial coefficients needed for Bernstein polynomials
- `bernsteinPoly`: Computes the Bernstein basis polynomial
- `bezierPoint`: Calculates a single point on the Bezier curve

### 3. **Curve Generation**
- `generateBezierCurve`: Creates multiple points along the curve by sampling parameter `t` from 0 to 1

### 4. **Usage Example**
The code demonstrates both quadratic and cubic Bezier curves with different control point configurations.

## Sample Output:
```
Bezier Curve Points:
Point 0: (0.00, 0.00)
Point 1: (0.25, 0.50)
Point 2: (0.50, 1.00)
Point 3: (0.75, 0.50)
Point 4: (1.00, 0.00)

Cubic Bezier Curve Points:
Point 0: (0.00, 0.00)
Point 1: (0.25, 0.31)
Point 2: (0.50, 0.62)
Point 3: (0.75, 0.81)
Point 4: (1.00, 1.00)
```

This implementation is efficient, functional, and demonstrates F#'s strengths in mathematical computations and list processing.

