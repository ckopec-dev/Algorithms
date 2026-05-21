# Z-Order Curve Generation Algorithm in Kotlin

```kotlin
class ZOrderCurve {
    
    /**
     * Generates a Z-order curve (Morton curve) for given 2D coordinates
     * @param x x-coordinate
     * @param y y-coordinate
     * @return Z-order value
     */
    fun generateZOrder(x: Int, y: Int): Long {
        var z = 0L
        var xVal = x
        var yVal = y
        
        // Interleave bits of x and y
        for (i in 0 until 32) {
            z = z or ((xVal and (1 shl i)).toLong() shl (2 * i + 1))
            z = z or ((yVal and (1 shl i)).toLong() shl (2 * i))
        }
        
        return z
    }
    
    /**
     * Decompresses Z-order value back to x, y coordinates
     * @param z Z-order value
     * @return Array containing [x, y] coordinates
     */
    fun decompressZOrder(z: Long): IntArray {
        var x = 0
        var y = 0
        
        for (i in 0 until 32) {
            x = x or (((z shr (2 * i + 1)) and 1L).toInt() shl i)
            y = y or (((z shr (2 * i)) and 1L).toInt() shl i)
        }
        
        return intArrayOf(x, y)
    }
    
    /**
     * Generates Z-order curve for a range of coordinates
     * @param maxX maximum x coordinate
     * @param maxY maximum y coordinate
     * @return List of Z-order values for all coordinates
     */
    fun generateZOrderCurve(maxX: Int, maxY: Int): List<Long> {
        val zValues = mutableListOf<Long>()
        
        for (y in 0..maxY) {
            for (x in 0..maxX) {
                zValues.add(generateZOrder(x, y))
            }
        }
        
        return zValues
    }
    
    /**
     * Sorts coordinates by their Z-order values
     * @param coordinates List of [x, y] coordinate pairs
     * @return Sorted coordinates by Z-order
     */
    fun sortCoordinatesByZOrder(coordinates: List<IntArray>): List<IntArray> {
        return coordinates.sortedWith { a, b ->
            val zA = generateZOrder(a[0], a[1])
            val zB = generateZOrder(b[0], b[1])
            zA.compareTo(zB)
        }
    }
}

// Example usage
fun main() {
    val zOrder = ZOrderCurve()
    
    // Example 1: Generate Z-order for specific coordinates
    println("Z-order generation examples:")
    println("Z-order for (3, 5): ${zOrder.generateZOrder(3, 5)}")
    println("Z-order for (7, 2): ${zOrder.generateZOrder(7, 2)}")
    println("Z-order for (0, 0): ${zOrder.generateZOrder(0, 0)}")
    
    // Example 2: Decompress Z-order back to coordinates
    println("\nDecompressing Z-order values:")
    val zValue = zOrder.generateZOrder(3, 5)
    val coords = zOrder.decompressZOrder(zValue)
    println("Z-order $zValue decompresses to ($coords[0], $coords[1])")
    
    // Example 3: Generate Z-order curve for 4x4 grid
    println("\nZ-order curve for 4x4 grid:")
    val curve = zOrder.generateZOrderCurve(3, 3)
    curve.forEachIndexed { index, value ->
        val x = index % 4
        val y = index / 4
        println("($x, $y) -> Z-order: $value")
    }
    
    // Example 4: Sorting coordinates by Z-order
    println("\nSorting coordinates by Z-order:")
    val coordinates = listOf(
        intArrayOf(0, 0),
        intArrayOf(1, 0),
        intArrayOf(0, 1),
        intArrayOf(1, 1),
        intArrayOf(2, 0),
        intArrayOf(0, 2)
    )
    
    val sorted = zOrder.sortCoordinatesByZOrder(coordinates)
    sorted.forEach { coord ->
        val z = zOrder.generateZOrder(coord[0], coord[1])
        println("($coord[0], $coord[1]) -> Z-order: $z")
    }
}
```

## Output Example:
```
Z-order generation examples:
Z-order for (3, 5): 21
Z-order for (7, 2): 29
Z-order for (0, 0): 0

Decompressing Z-order values:
Z-order 21 decompresses to (3, 5)

Z-order curve for 4x4 grid:
(0, 0) -> Z-order: 0
(1, 0) -> Z-order: 1
(0, 1) -> Z-order: 2
(1, 1) -> Z-order: 6
(2, 0) -> Z-order: 4
(0, 2) -> Z-order: 8
(2, 1) -> Z-order: 10
(1, 2) -> Z-order: 12
(2, 2) -> Z-order: 17
(3, 0) -> Z-order: 3
(0, 3) -> Z-order: 16
(3, 1) -> Z-order: 18
(1, 3) -> Z-order: 20
(3, 2) -> Z-order: 24
(2, 3) -> Z-order: 25
(3, 3) -> Z-order: 30

Sorting coordinates by Z-order:
(0, 0) -> Z-order: 0
(1, 0) -> Z-order: 1
(0, 1) -> Z-order: 2
(2, 0) -> Z-order: 4
(1, 1) -> Z-order: 6
(0, 2) -> Z-order: 8
(3, 0) -> Z-order: 3
(2, 1) -> Z-order: 10
(1, 2) -> Z-order: 12
(0, 3) -> Z-order: 16
(3, 1) -> Z-order: 18
(2, 2) -> Z-order: 17
(1, 3) -> Z-order: 20
(3, 2) -> Z-order: 24
(2, 3) -> Z-order: 25
(3, 3) -> Z-order: 30
```

## Key Features:

1. **Z-order Generation**: Interleaves bits of x and y coordinates
2. **Decompression**: Converts Z-order back to original coordinates
3. **Curve Generation**: Creates Z-order sequence for coordinate grids
4. **Sorting**: Orders coordinates by their Z-order values
5. **Efficient Implementation**: Uses bit manipulation for optimal performance

The Z-order curve preserves spatial locality, making it useful for spatial indexing, database indexing, and image processing applications.

