## De Boor’s Algorithm (B‑Spline Curve Evaluation) – C++ Example  

Below is a **self‑contained, modern‑C++ (C++17) implementation** that evaluates a **uniform B‑spline curve** of degree `p` at a parametric value `u`.  
The code follows the classic recursive formulation of de Boor’s algorithm, but it is written iteratively for speed and clarity.

```cpp
/**
 * @file deboor.cpp
 * @brief Minimal example of de Boor's algorithm for B‑spline curve evaluation.
 *
 * Compile with:
 *     g++ -std=c++17 -O2 -Wall -pedantic deboor.cpp -o deboor
 *
 * The program evaluates a simple 2‑D quadratic B‑spline (degree = 2) and prints
 * a few points on the curve.
 */

#include <iostream>
#include <vector>
#include <array>
#include <cassert>
#include <iomanip>   // std::setprecision

// ---------------------------------------------------------------------------
// Helper type: 2‑D point (you can replace this with any vector type you like)
// ---------------------------------------------------------------------------
using Point2 = std::array<double, 2>;

// ---------------------------------------------------------------------------
// Linear interpolation (lerp) between two points.
// ---------------------------------------------------------------------------
inline Point2 lerp(const Point2& a, const Point2& b, double t)
{
    return {
        a[0] + t * (b[0] - a[0]),
        a[1] + t * (b[1] - a[1])
    };
}

// ---------------------------------------------------------------------------
// De Boor's algorithm.
//   * `p`   – degree of the B‑spline (≥0)
//   * `knots` – non‑decreasing knot vector (size = n + p + 1)
//   * `ctrl`  – control points (size = n)
//   * `u`    – parametric coordinate at which to evaluate the curve
// Returns the point C(u) on the B‑spline.
// ---------------------------------------------------------------------------
Point2 deBoor(const std::vector<double>& knots,
              const std::vector<Point2>& ctrl,
              std::size_t p,
              double u)
{
    const std::size_t n = ctrl.size();               // number of control points
    assert(p <= n && "degree must be <= number of control points");
    assert(knots.size() == n + p + 1 && "knot vector size mismatch");

    // ---------------------------------------------------------------
    // 1) Find the knot span that contains u.
    //    We look for i such that knots[i] ≤ u < knots[i+1] (or u == last knot).
    // ---------------------------------------------------------------
    std::size_t i = p;                               // start search after the p‑th knot
    while (i < n + p && !(u >= knots[i] && u < knots[i+1]))
        ++i;
    // Handle the case u == knots.back() (the curve is defined at the end)
    if (u == knots.back())
        i = n + p - 1;                               // last valid span

    // ---------------------------------------------------------------
    // 2) Initialize the control points for this span:
    //    d[j] = ctrl[i - p + j]   for j = 0 … p
    // ---------------------------------------------------------------
    std::vector<Point2> d(p + 1);
    for (std::size_t j = 0; j <= p; ++j)
        d[j] = ctrl[i - p + j];

    // ---------------------------------------------------------------
    // 3) Perform the de Boor iteration.
    //    For r = 1 … p:
    //        for j = p … r:
    //            α = (u - knots[i + j - r]) / (knots[i + j + 1 - r] - knots[i + j - r])
    //            d[j] = (1-α) * d[j-1] + α * d[j]
    // ---------------------------------------------------------------
    for (std::size_t r = 1; r <= p; ++r)
    {
        for (std::size_t j = p; j >= r; --j)
        {
            double left  = knots[i + j - r];
            double right = knots[i + j + 1 - r];
            double alpha = (right == left) ? 0.0   // avoid division by zero for duplicated knots
                                            : (u - left) / (right - left);

            d[j][0] = (1.0 - alpha) * d[j-1][0] + alpha * d[j][0];
            d[j][1] = (1.0 - alpha) * d[j-1][1] + alpha * d[j][1];
        }
    }

    // The result is in d[p]
    return d[p];
}

// ---------------------------------------------------------------------------
// Demo: a quadratic (degree=2) closed B‑spline forming a simple triangle.
// ---------------------------------------------------------------------------
int main()
{
    // Control points (counter‑clockwise)
    std::vector<Point2> ctrl = {
        {0.0, 0.0},
        {2.0, 0.0},
        {1.0, 2.0}
    };

    const std::size_t p = 2;                     // degree
    // For an open uniform knot vector with clamped ends:
    // size = n + p + 1 = 3 + 2 + 1 = 6
    std::vector<double> knots = {0, 0, 0, 1, 1, 1};

    std::cout << std::fixed << std::setprecision(6);
    std::cout << "Evaluating quadratic B‑spline (degree = " << p << ")\n";
    std::cout << "Control points:\n";
    for (const auto& pt : ctrl)
        std::cout << "  (" << pt[0] << ", " << pt[1] << ")\n";

    std::cout << "\nSample points on the curve (u from 0 to 1):\n";
    const std::size_t samples = 11;
    for (std::size_t k = 0; k < samples; ++k)
    {
        double u = static_cast<double>(k) / (samples - 1); // 0 … 1
        Point2 Pt = deBoor(knots, ctrl, p, u);
        std::cout << "  u = " << std::setw(5) << u
                  << " → C(u) = (" << Pt[0] << ", " << Pt[1] << ")\n";
    }

    return 0;
}
```

### How the Code Works
| Step | What Happens | Why It Matters |
|------|--------------|----------------|
| **Knot‑span search** | Finds `i` such that `u ∈ [knots[i], knots[i+1])`. | De Boor only needs the `p+1` control points that influence the curve at `u`. |
| **Copy relevant control points** | `d[j] = ctrl[i-p+j]` for `j = 0…p`. | Starts the triangular computation with the correct subset of control points. |
| **Iterative refinement** | For each level `r = 1…p` compute the blended points `d[j]` using the linear interpolation factor `α`. | Implements the recursion `C_i^r(u) = (1‑α) C_{i‑1}^{r‑1}(u) + α C_i^{r‑1}(u)`. |
| **Result** | After `p` iterations, `d[p]` holds `C(u)`. | The final blended point is the point on the B‑spline. |

### Remarks & Extensions
* **Non‑uniform knot vectors** – The same code works; just supply a non‑decreasing `knots` vector.  
* **Higher dimensions** – Replace `Point2` with a struct/templated vector (e.g., `std::array<double,3>` for 3‑D).  
* **Closed / periodic splines** – Duplicate the first `p` control points at the end and use a periodic knot vector.  
* **Performance** – For many evaluations you can pre‑compute the denominator `(knots[i+j+1‑r] - knots[i+j‑r])` or use a lookup table for the span `i`.  

Compile and run the program; you’ll see a set of points that trace the quadratic B‑spline defined by the three control points—a simple triangle. Feel free to change `ctrl`, `p`, and `knots` to experiment with different curves!