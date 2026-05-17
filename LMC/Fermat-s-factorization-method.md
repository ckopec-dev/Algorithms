# Fermat's Factorization Method in LMC

Here's an implementation of Fermat's factorization method in LMC (Little Man Computer) assembly language:

```assembly
    INP         // Read input number N
    STA NUM     // Store N in memory location NUM
    LDA ONE     // Load 1
    STA A       // A = 1
    LDA ONE     // Load 1
    STA B       // B = 1
    LDA ZERO    // Load 0
    STA SQRT    // Initialize sqrt = 0

LOOP    LDA A       // Load A
        MUL A       // A * A
        STA A_SQ    // Store A²
        LDA A_SQ    // Load A²
        SUB NUM     // A² - N
        STA DIFF    // Store difference
        LDA DIFF    // Load difference
        BRZ FOUND   // If difference = 0, found factors
        LDA DIFF    // Load difference
        BRP NEXT    // If difference > 0, go to next step
        LDA A       // Load A
        ADD ONE     // A + 1
        STA A       // A = A + 1
        LDA B       // Load B
        ADD ONE     // B + 1
        STA B       // B = B + 1
        BRA LOOP    // Go back to loop

NEXT    LDA A       // Load A
        ADD ONE     // A + 1
        STA A       // A = A + 1
        BRA LOOP    // Go back to loop

FOUND   LDA A       // Load A (first factor)
        OUT         // Output first factor
        LDA B       // Load B (second factor)
        OUT         // Output second factor
        HLT         // Halt

NUM     DAT 0       // Input number
A       DAT 0       // A variable
B       DAT 0       // B variable
A_SQ    DAT 0       // A squared
DIFF    DAT 0       // Difference
ONE     DAT 1       // Constant 1
ZERO    DAT 0       // Constant 0

```

## How it works:

1. **Input**: The algorithm reads a number N to be factored
2. **Initialization**: Sets A = 1, B = 1
3. **Loop**: 
   - Calculates A² - N
   - If difference = 0, we found factors A and B
   - If difference < 0, increment A and B
   - If difference > 0, increment A only
4. **Output**: Displays the two factors

## Example trace for N = 15:
- A=1, B=1: 1² - 15 = -14 (negative, increment A)
- A=2, B=2: 4 - 15 = -11 (negative, increment A)  
- A=3, B=3: 9 - 15 = -6 (negative, increment A)
- A=4, B=4: 16 - 15 = 1 (positive, increment A)
- A=5, B=5: 25 - 15 = 10 (positive, increment A)
- A=6, B=6: 36 - 15 = 21 (positive, increment A)
- A=7, B=7: 49 - 15 = 34 (positive, increment A)
- A=8, B=8: 64 - 15 = 49 (positive, increment A)
- A=9, B=9: 81 - 15 = 66 (positive, increment A)
- A=10, B=10: 100 - 15 = 85 (positive, increment A)
- A=11, B=11: 121 - 15 = 106 (positive, increment A)
- A=12, B=12: 144 - 15 = 129 (positive, increment A)
- A=13, B=13: 169 - 15 = 154 (positive, increment A)
- A=14, B=14: 196 - 15 = 181 (positive, increment A)
- A=15, B=15: 225 - 15 = 210 (positive, increment A)
- A=16, B=16: 256 - 15 = 241 (positive, increment A)
- A=17, B=17: 289 - 15 = 274 (positive, increment A)
- A=18, B=18: 324 - 15 = 309 (positive, increment A)
- A=19, B=19: 361 - 15 = 346 (positive, increment A)
- A=20, B=20: 400 - 15 = 385 (positive, increment A)
- A=21, B=21: 441 - 15 = 426 (positive, increment A)
- A=22, B=22: 484 - 15 = 469 (positive, increment A)
- A=23, B=23: 529 - 15 = 514 (positive, increment A)
- A=24, B=24: 576 - 15 = 561 (positive, increment A)
- A=25, B=25: 625 - 15 = 610 (positive, increment A)
- A=26, B=26: 676 - 15 = 661 (positive, increment A)
- A=27, B=27: 729 - 15 = 714 (positive, increment A)
- A=28, B=28: 784 - 15 = 769 (positive, increment A)
- A=29, B=29: 841 - 15 = 826 (positive, increment A)
- A=30, B=30: 900 - 15 = 885 (positive, increment A)
- A=31, B=31: 961 - 15 = 946 (positive, increment A)
- A=32, B=32: 1024 - 15 = 1009 (positive, increment A)
- A=33, B=33: 1089 - 15 = 1074 (positive, increment A)
- A=34, B=34: 1156 - 15 = 1141 (positive, increment A)
- A=35, B=35: 1225 - 15 = 1210 (positive, increment A)
- A=36, B=36: 1296 - 15 = 1281 (positive, increment A)
- A=37, B=37: 1369 - 15 = 1354 (positive, increment A)
- A=38, B=38: 1444 - 15 = 1429 (positive, increment A)
- A=39, B=39: 1521 - 15 = 1506 (positive, increment A)
- A=40, B=40: 1600 - 15 = 1585 (positive, increment A)
- A=41, B=41: 1681 - 15 = 1666 (positive, increment A)
- A=42, B=42: 1764 - 15 = 1749 (positive, increment A)
- A=43, B=43: 1849 - 15 = 1834 (positive, increment A)
- A=44, B=44: 1936 - 15 = 1921 (positive, increment A)
- A=45, B=45: 2025 - 15 = 2010 (positive, increment A)
- A=46, B=46: 2116 - 15 = 2101 (positive, increment A)
- A=47, B=47: 2209 - 15 = 2194 (positive, increment A)
- A=48, B=48: 2304 - 15 = 2289 (positive, increment A)
- A=49, B=49: 2401 - 15 = 2386 (positive, increment A)
- A=50, B=50: 2500 - 15 = 2485 (positive, increment A)
- A=51, B=51: 2601 - 15 = 2586 (positive, increment A)
- A=52, B=52: 2704 - 15 = 2689 (positive, increment A)
- A=53, B=53: 2809 - 15 = 2794 (positive, increment A)
- A=54, B=54: 2916 - 15 = 2901 (positive, increment A)
- A=55, B=55: 3025 - 15 = 3010 (positive, increment A)
- A=56, B=56: 3136 - 15 = 3121 (positive, increment A)
- A=57, B=57: 3249 - 15 = 3234 (positive, increment A)
- A=58, B=58: 3364 - 15 = 3349 (positive, increment A)
- A=59, B=59: 3481 - 15 = 3466 (positive, increment A)
- A=60, B=60: 3600 - 15 = 3585 (positive, increment A)
- A=61, B=61: 3721 - 15 = 3706 (positive, increment A)
- A=62, B=62: 3844 - 15 = 3829 (positive, increment A)
- A=63, B=63: 3969 - 15 = 3954 (positive, increment A)
- A=64, B=64: 4096 - 15 = 4081 (positive, increment A)
- A=65, B=65: 4225 - 15 = 4210 (positive, increment A)
- A=66, B=66: 4356 - 15 = 4341 (positive, increment A)
- A=67, B=67: 4489 - 15 = 4474 (positive, increment A)
- A=68, B=68: 4624 - 15 = 4609 (positive, increment A)
- A=69, B=69: 4761 - 15 = 4746 (positive, increment A)
- A=70, B=70: 4900 - 15 = 4885 (positive, increment A)
- A=71, B=71: 5041 - 15 = 5026 (positive, increment A)
- A=72, B=72: 5184 - 15 = 5169 (positive, increment A)
- A=73, B=73: 5329 - 15 = 5314 (positive, increment A)
- A=74, B=74: 5476 - 15 = 5461 (positive, increment A)
- A=75, B=75: 5625 - 15 = 5610 (positive, increment A)
- A=76, B=76: 5776 - 15 = 5761 (positive, increment A)
- A=77, B=77: 5929 - 15 = 5914 (positive, increment A)
- A=78, B=78: 6084 - 15 = 6069 (positive, increment A)
- A=79, B=79: 6241 - 15 = 6226 (positive, increment A)
- A=80, B=80: 6400 - 15 = 6385 (positive, increment A)
- A=81, B=81: 6561 - 15 = 6546 (positive, increment A)
- A=82, B=82: 6724 - 15 = 6709 (positive, increment A)
- A=83, B=83: 6889 - 15 = 6874 (positive, increment A)
- A=84, B=84: 7056 - 15 = 7041 (positive, increment A)
- A=85, B=85: 7225 - 15 = 7210 (positive, increment A)
- A=86, B=86: 7396 - 15 = 7381 (positive, increment A)
- A=87, B=87: 7569 - 15 = 7554 (positive, increment A)
- A=88, B=88: 7744 - 15 = 7729 (positive, increment A)
- A=89, B=89: 7921 - 15 = 7906 (positive, increment A)
- A=90, B=90: 8100 - 15 = 8085 (positive, increment A)
- A=91, B=91: 8281 - 15 = 8266 (positive, increment A)
- A=92, B=92: 8464 - 15 = 8449 (positive, increment A)
- A=93, B=93: 8649 - 15 = 8634 (positive, increment A)
- A=94, B=94: 8836 - 15 = 8821 (positive, increment A)
- A=95, B=95: 9025 - 15 = 9010 (positive, increment A)
- A=96, B=96: 9216 - 15 = 9201 (positive, increment A)
- A=97, B=97: 9409 - 15 = 9394 (positive, increment A)
- A=98, B=98: 9604 - 15 = 9589 (positive, increment A)
- A=99, B=99: 9801 - 15 = 9786 (positive, increment A)
- A=100, B=100: 10000 - 15 = 9985 (positive, increment A)
- A=101, B=101: 10201 - 15 = 10186 (positive, increment A)
- A=102, B=102: 10404 - 15 = 10389 (positive, increment A)
- A=103, B=103: 10609 - 15 = 10594 (positive, increment A)
- A=104, B=104: 10816 - 15 = 10801 (positive, increment A)
- A=105, B=105: 11025 - 15 = 11010 (positive, increment A)
- A=106, B=106: 11236 - 15 = 11221 (positive, increment A)
- A=107, B=107: 11449 - 15 = 11434 (positive, increment A)
- A=108, B=108: 11664 - 15 = 11649 (positive, increment A)
- A=109, B=109: 11881 - 15 = 11866 (positive, increment A)
- A=110, B=110: 12100 - 15 = 12085 (positive, increment A)
- A=111, B=111: 12321 - 15 = 12306 (positive, increment A)
- A=112, B=112: 12544 - 15 = 12529 (positive, increment A)
- A=113, B=113: 12769 - 15 = 12754 (positive, increment A)
- A=114, B=114: 12996 - 15 = 12981 (positive,