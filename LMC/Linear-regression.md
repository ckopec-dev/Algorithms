# Linear Regression in LMC (Little Man Computer)

Here's an example of a simple linear regression algorithm implemented in LMC. This implementation calculates the slope and y-intercept of a line given two points.

```assembly
    INP         // Input x1
    STA X1      // Store x1
    INP         // Input y1
    STA Y1      // Store y1
    INP         // Input x2
    STA X2      // Store x2
    INP         // Input y2
    STA Y2      // Store y2

    // Calculate slope: m = (y2-y1)/(x2-x1)
    LDA Y2      // Load y2
    SUB Y1      // Subtract y1
    STA DELTA_Y // Store (y2-y1)
    
    LDA X2      // Load x2
    SUB X1      // Subtract x1
    STA DELTA_X // Store (x2-x1)
    
    LDA DELTA_Y // Load (y2-y1)
    DIV DELTA_X // Divide by (x2-x1)
    STA SLOPE   // Store slope
    
    // Calculate y-intercept: b = y1 - m*x1
    LDA SLOPE   // Load slope
    MUL X1      // Multiply by x1
    STA M_X1    // Store m*x1
    
    LDA Y1      // Load y1
    SUB M_X1    // Subtract m*x1
    STA INTERCEPT // Store intercept
    
    // Output results
    LDA SLOPE   // Load slope
    OUT         // Output slope
    LDA INTERCEPT // Load intercept
    OUT         // Output intercept

    HLT         // Halt

X1      DAT     // x1 storage
Y1      DAT     // y1 storage
X2      DAT     // x2 storage
Y2      DAT     // y2 storage
DELTA_Y DAT     // (y2-y1) storage
DELTA_X DAT     // (x2-x1) storage
SLOPE   DAT     // slope storage
M_X1    DAT     // m*x1 storage
INTERCEPT DAT   // y-intercept storage
```

## How it works:

1. **Input**: Takes 4 values (x1, y1, x2, y2) representing two points
2. **Slope calculation**: Computes m = (y2-y1)/(x2-x1)
3. **Intercept calculation**: Computes b = y1 - m*x1
4. **Output**: Displays the slope and y-intercept

## Example usage:
- Input: 1, 2, 3, 4
- This represents points (1,2) and (3,4)
- Output: slope = 1, intercept = 1
- Resulting line: y = x + 1

Note: This is a simplified implementation. A full linear regression would require more complex algorithms to handle multiple variables and minimize error functions.

