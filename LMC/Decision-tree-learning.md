# Decision Tree Learning in LMC

Here's an example implementation of a simple decision tree learning algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Decision Tree Learning Algorithm in LMC
; This example demonstrates a basic decision tree for classifying weather conditions

    ORG 100
    DAT 0          ; Input feature 1 (temperature)
    DAT 0          ; Input feature 2 (humidity)
    DAT 0          ; Input feature 3 (wind speed)
    DAT 0          ; Decision result (1=sunny, 2=rainy, 3=cloudy)
    
; Main program
MAIN    INP            ; Input first feature
        STA FEATURE1
        INP            ; Input second feature
        STA FEATURE2
        INP            ; Input third feature
        STA FEATURE3
        
        ; Start decision tree
        LDA FEATURE1   ; Load temperature
        SUB 20         ; Compare with threshold (20)
        BRP TEMP_HIGH  ; If temp >= 20, go to high temp branch
        
        ; Low temperature branch
        LDA FEATURE2   ; Load humidity
        SUB 60         ; Compare with threshold (60)
        BRP HUMID_HIGH ; If humidity >= 60, go to high humidity
        LDA 3          ; Return cloudy (3)
        OUT
        HALT
        
TEMP_HIGH
        LDA FEATURE3   ; Load wind speed
        SUB 10         ; Compare with threshold (10)
        BRP WIND_HIGH  ; If wind >= 10, go to high wind
        LDA 1          ; Return sunny (1)
        OUT
        HALT
        
WIND_HIGH
        LDA 2          ; Return rainy (2)
        OUT
        HALT
        
HUMID_HIGH
        LDA 2          ; Return rainy (2)
        OUT
        HALT
        
; Data storage
FEATURE1 DAT 0
FEATURE2 DAT 0
FEATURE3 DAT 0

    END MAIN
```

## Algorithm Explanation

This LMC implementation demonstrates a simple decision tree for weather classification:

1. **Input Processing**: Takes three features (temperature, humidity, wind speed)
2. **Decision Logic**: 
   - First compares temperature with threshold (20)
   - If high temperature: compares wind speed with threshold (10)
   - If low temperature: compares humidity with threshold (60)
3. **Output**: Returns classification (1=sunny, 2=rainy, 3=cloudy)

## Key LMC Concepts Used

- **INP/OUT**: Input/output operations for feature values
- **STA/LDA**: Store and load operations for data manipulation
- **SUB/BRP**: Subtraction and branch if positive for comparisons
- **HALT**: Program termination

## Sample Execution

Input: Temperature=25, Humidity=40, Wind=15
1. Temperature = 25 ≥ 20 → Take high temp branch
2. Wind = 15 ≥ 10 → Take high wind branch  
3. Output: 2 (rainy)

This demonstrates the core decision tree logic in LMC's limited but functional instruction set.

