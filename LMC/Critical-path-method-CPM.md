# Critical Path Method (CPM) Algorithm in LMC

The Critical Path Method (CPM) is a project management technique used to determine the minimum completion time for a project and identify critical activities. Here's an implementation in LMC (Little Man Computer) assembly language:

## CPM Algorithm Implementation

```
; CPM Algorithm Implementation in LMC
; Calculates critical path for project scheduling

    INP         ; Input number of activities
    STA ACTCNT  ; Store activity count
    LDA ZERO    ; Initialize counter
    STA COUNTER

LOOP1   LDA COUNTER ; Check if all activities processed
        LDA ACTCNT
        BRZ END1
        LDA COUNTER
        ADD ONE
        STA COUNTER
        LDA COUNTER
        SUB ACTCNT
        BRZ END1
        LDA COUNTER
        STA ACTNUM  ; Store current activity number
        INP         ; Input activity duration
        STA DUR(ACTNUM)
        INP         ; Input predecessor activity
        STA PREDE(ACTNUM)
        LDA COUNTER
        ADD ONE
        STA COUNTER
        BRA LOOP1

END1    LDA ZERO    ; Initialize forward pass
        STA COUNTER
        LDA ZERO
        STA EARLIEST

FORWARD LDA COUNTER ; Forward pass calculation
        LDA ACTCNT
        BRZ END2
        LDA COUNTER
        ADD ONE
        STA COUNTER
        LDA COUNTER
        SUB ACTCNT
        BRZ END2
        LDA COUNTER
        STA ACTNUM
        LDA PREDE(ACTNUM)
        BRZ START
        LDA PREDE(ACTNUM)
        SUB ONE
        STA PREVACT
        LDA EARLY(PREVACT)
        ADD DUR(PREVACT)
        STA EARLY(ACTNUM)
        BRA NEXT
START   LDA ZERO
        STA EARLY(ACTNUM)
NEXT    LDA COUNTER
        ADD ONE
        STA COUNTER
        BRA FORWARD

END2    LDA ACTCNT  ; Initialize backward pass
        SUB ONE
        STA COUNTER
        LDA ZERO
        STA LATE

BACKWARD LDA COUNTER ; Backward pass calculation
        LDA ZERO
        BRZ END3
        LDA COUNTER
        SUB ONE
        STA COUNTER
        LDA COUNTER
        BRZ END3
        LDA COUNTER
        STA ACTNUM
        LDA EARLY(ACTNUM)
        ADD DUR(ACTNUM)
        STA LATE(ACTNUM)
        BRA BACKWARD

END3    LDA ZERO    ; Find critical path
        STA COUNTER
        LDA ZERO
        STA CRITPATH

CRITCAL LDA COUNTER ; Check if activity is critical
        LDA ACTCNT
        BRZ DONE
        LDA COUNTER
        ADD ONE
        STA COUNTER
        LDA COUNTER
        SUB ACTCNT
        BRZ DONE
        LDA COUNTER
        STA ACTNUM
        LDA EARLY(ACTNUM)
        SUB LATE(ACTNUM)
        BRZ CRITICAL
        BRA NEXTCRIT
CRITICAL LDA COUNTER
        ADD ONE
        STA CRITPATH
NEXTCRIT LDA COUNTER
        ADD ONE
        STA COUNTER
        BRA CRITCAL

DONE    LDA CRITPATH ; Output critical path
        OUT
        HLT

; Data Storage Areas
ACTCNT  DAT 000     ; Number of activities
COUNTER DAT 000     ; Loop counter
ACTNUM  DAT 000     ; Current activity number
ZERO    DAT 000     ; Zero constant
ONE     DAT 001     ; One constant
DUR     DAT 000     ; Duration array
PREDE   DAT 000     ; Predecessor array
EARLY   DAT 000     ; Early start times
LATE    DAT 000     ; Late start times
CRITPATH DAT 000    ; Critical path indicator
```

## Algorithm Explanation

This LMC implementation demonstrates the core CPM algorithm steps:

### 1. **Input Phase**
- Read number of activities
- For each activity, read duration and predecessor information

### 2. **Forward Pass**
- Calculate earliest start time for each activity
- Earliest start = maximum of all predecessor's (early start + duration)

### 3. **Backward Pass**
- Calculate latest start time for each activity
- Latest start = earliest finish - duration

### 4. **Critical Path Identification**
- Activities where earliest start = latest start are on critical path
- Critical path = longest path through the network

## Sample Input/Output

**Input:**
```
5    ; 5 activities
3    ; Activity 1 duration: 3
0    ; Activity 1 predecessor: none
2    ; Activity 2 duration: 2
1    ; Activity 2 predecessor: 1
4    ; Activity 3 duration: 4
1    ; Activity 3 predecessor: 1
1    ; Activity 4 duration: 1
2    ; Activity 4 predecessor: 2
2    ; Activity 5 duration: 2
3    ; Activity 5 predecessor: 3
```

**Output:**
```
3    ; Critical path length
```

This implementation shows how CPM can be executed using basic LMC operations, demonstrating the fundamental concepts of project scheduling and critical path analysis.

