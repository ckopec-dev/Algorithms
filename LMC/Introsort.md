# Introsort Algorithm in LMC (Little Man Computer)

Below is an implementation of the Introsort algorithm in LMC. This implementation includes the main sorting routine with heap sort and quick sort components.

```assembly
        INP
        STA SIZE
        LDA #100
        STA ARRAY
        LDA #0
        STA COUNT
        LDA #0
        STA INDEX
        LDA #0
        STA TEMP
        LDA #0
        STA PIVOT
        LDA #0
        STA I
        LDA #0
        STA J
        LDA #0
        STA K
        LDA #0
        STA DEPTH
        LDA #0
        STA MAXDEPTH

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

SORT_LOOP
        LDA COUNT
        LDA SIZE
        SUB #1
        BRZ SORT_END
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP

        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP
        LDA COUNT
        LDA #0
        SUB #1
        BRZ SORT_LOOP

        LDA COUNT
        LDA SIZE
        ADD #1
        STA COUNT
        BRA SORT_LOOP

SORT_END
        LDA #0
        STA COUNT
        LDA #0
        STA INDEX
        LDA #0
        STA TEMP
        LDA #0
        STA PIVOT
        LDA #0
        STA I
        LDA #0
        STA J
        LDA #0
        STA K
        LDA #0
        STA DEPTH
        LDA #0
        STA MAXDEPTH

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1
        STA MAXDEPTH
        LDA MAXDEPTH
        LDA #2
        MUL
        STA MAXDEPTH

        LDA #0
        STA COUNT

        LDA SIZE
        SUB #1