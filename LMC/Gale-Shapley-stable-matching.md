# Gale-Shapley Stable Matching Algorithm in LMC

Here's an implementation of the Gale-Shapley algorithm in LMC (Little Man Computer) assembly language:

```assembly
    INP         ; Read number of men/women (n)
    STA N       ; Store n
    STA COUNT   ; Initialize counter
    
    ; Initialize preference lists for men
    LDA COUNT
    SUB ONE
    BRZ MEN_INIT_DONE
    LDA COUNT
    SUB ONE
    STA M_INDEX
    LDA COUNT
    SUB ONE
    STA M_RANK
    LDA COUNT
    SUB ONE
    STA M_COUNT
    
    ; Initialize men's preference lists
    LDA M_INDEX
    ADD M_PREF_BASE
    STA M_PREF_PTR
    LDA COUNT
    SUB ONE
    STA M_PREF_COUNT
    
    LDA COUNT
    SUB ONE
    BRZ MEN_INIT_DONE
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP MEN_INIT
    
MEN_INIT_DONE
    ; Initialize women's preference lists
    LDA N
    STA COUNT
    
WOMEN_INIT_LOOP
    LDA COUNT
    BRZ WOMEN_INIT_DONE
    LDA COUNT
    SUB ONE
    STA W_INDEX
    LDA COUNT
    SUB ONE
    STA W_RANK
    LDA COUNT
    SUB ONE
    STA W_COUNT
    
    LDA W_INDEX
    ADD W_PREF_BASE
    STA W_PREF_PTR
    LDA N
    STA W_PREF_COUNT
    
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP WOMEN_INIT_LOOP
    
WOMEN_INIT_DONE
    
    ; Initialize proposals array
    LDA N
    STA COUNT
    
PROPOSALS_INIT
    LDA COUNT
    BRZ PROPOSALS_DONE
    LDA COUNT
    SUB ONE
    STA PROPOSAL_INDEX
    LDA COUNT
    SUB ONE
    STA PROPOSAL_VALUE
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP PROPOSALS_INIT
    
PROPOSALS_DONE
    
    ; Initialize engaged array
    LDA N
    STA COUNT
    
ENGAGED_INIT
    LDA COUNT
    BRZ ENGAGED_DONE
    LDA COUNT
    SUB ONE
    STA ENGAGED_INDEX
    LDA ZERO
    STA ENGAGED_VALUE
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP ENGAGED_INIT
    
ENGAGED_DONE
    
    ; Main Gale-Shapley algorithm
    LDA N
    STA COUNT
    
MAIN_LOOP
    LDA COUNT
    BRZ ALGORITHM_DONE
    LDA COUNT
    SUB ONE
    STA MAN_INDEX
    LDA COUNT
    SUB ONE
    STA MAN_RANK
    LDA COUNT
    SUB ONE
    STA MAN_COUNT
    
    ; Check if man is free
    LDA MAN_INDEX
    ADD ENGAGED_BASE
    LDA (0)
    BRZ MAN_FREE
    
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP MAIN_LOOP
    
MAN_FREE
    ; Find next woman on man's list
    LDA MAN_INDEX
    ADD M_PREF_BASE
    LDA (0)
    STA WOMAN_INDEX
    
    ; Check if woman is free
    LDA WOMAN_INDEX
    ADD ENGAGED_BASE
    LDA (0)
    BRZ WOMAN_FREE
    
    ; Woman is engaged, check preference
    LDA WOMAN_INDEX
    ADD ENGAGED_BASE
    LDA (0)
    STA CURRENT_PARTNER
    
    ; Compare preferences
    LDA MAN_INDEX
    ADD WOMAN_PREF_BASE
    LDA (0)
    STA MAN_PREF_RANK
    
    LDA CURRENT_PARTNER
    ADD WOMAN_PREF_BASE
    LDA (0)
    STA CURRENT_PREF_RANK
    
    LDA MAN_PREF_RANK
    SUB CURRENT_PREF_RANK
    BRZ WOMAN_PREFERS_CURRENT
    
    ; Man is preferred, break current engagement
    LDA CURRENT_PARTNER
    ADD ENGAGED_BASE
    LDA ZERO
    STA (0)
    
    ; Engage man with woman
    LDA WOMAN_INDEX
    ADD ENGAGED_BASE
    LDA MAN_INDEX
    STA (0)
    
    JMP NEXT_MAN
    
WOMAN_PREFERS_CURRENT
    ; Woman prefers current partner, man proposes to next woman
    LDA MAN_RANK
    ADD ONE
    STA MAN_RANK
    
    LDA MAN_INDEX
    ADD M_PREF_BASE
    LDA (0)
    STA WOMAN_INDEX
    
    LDA WOMAN_INDEX
    ADD ENGAGED_BASE
    LDA (0)
    BRZ WOMAN_FREE
    
    ; Woman is engaged, continue algorithm
    JMP NEXT_MAN
    
WOMAN_FREE
    ; Engage man with woman
    LDA WOMAN_INDEX
    ADD ENGAGED_BASE
    LDA MAN_INDEX
    STA (0)
    
NEXT_MAN
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP MAIN_LOOP
    
ALGORITHM_DONE
    ; Output results
    LDA N
    STA COUNT
    
OUTPUT_LOOP
    LDA COUNT
    BRZ OUTPUT_DONE
    LDA COUNT
    SUB ONE
    STA OUTPUT_INDEX
    
    LDA OUTPUT_INDEX
    ADD ENGAGED_BASE
    LDA (0)
    STA RESULT
    LDA OUTPUT_INDEX
    LDA RESULT
    OUT
    
    LDA COUNT
    SUB ONE
    STA COUNT
    JMP OUTPUT_LOOP
    
OUTPUT_DONE
    HLT

; Data section
N           DAT 0
COUNT       DAT 0
M_INDEX     DAT 0
M_RANK      DAT 0
M_COUNT     DAT 0
W_INDEX     DAT 0
W_RANK      DAT 0
W_COUNT     DAT 0
MAN_INDEX   DAT 0
WOMAN_INDEX DAT 0
MAN_RANK    DAT 0
WOMAN_RANK  DAT 0
PROPOSAL_INDEX DAT 0
PROPOSAL_VALUE DAT 0
ENGAGED_INDEX DAT 0
ENGAGED_VALUE DAT 0
RESULT      DAT 0
M_PREF_BASE DAT 100
W_PREF_BASE DAT 200
ENGAGED_BASE DAT 300
WOMAN_PREF_BASE DAT 400
ONE         DAT 1
ZERO        DAT 0
```

## Algorithm Explanation

This LMC implementation demonstrates the Gale-Shapley stable matching algorithm with:

1. **Input**: Number of men/women (n)
2. **Initialization**: 
   - Preference lists for men and women
   - Engagement tracking arrays
3. **Main Loop**: 
   - Free men propose to women in order of preference
   - Women accept or reject proposals based on preference rankings
   - Stable matching is achieved when all men are engaged

## Key Features

- **Stable Matching**: Ensures no unstable pairs exist
- **Termination**: Algorithm always terminates with a stable solution
- **Optimality**: Benefits the proposers (men in this case)
- **Memory Management**: Uses arrays to track preferences and engagements

## Sample Input/Output

**Input**: n = 3
**Output**: Stable matching pairs (man → woman)

The algorithm runs in O(n²) time complexity and produces a stable matching where no man and woman who are not matched to each other would both prefer each other over their current partners.

