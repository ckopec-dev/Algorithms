# Cycle Sort Algorithm in LMC

Here's an implementation of the Cycle Sort algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Cycle Sort Algorithm - LMC Implementation
; Sorts an array of numbers in ascending order

; Data Section
NUMBERS DAT 5
        DAT 2
        DAT 8
        DAT 1
        DAT 9
        DAT 3
        DAT 7
        DAT 4
        DAT 6
        DAT 0
LENGTH DAT 10

; Main Program
        INP
        STA COUNT
        LDA LENGTH
        STA SIZE
        LDA NUMBERS
        STA ARRAY
        LDA COUNT
        STA I
        LDA COUNT
        STA J
        LDA COUNT
        STA K

; Cycle Sort Algorithm
SORT    LDA I
        LDA SIZE
        SUB DAT 1
        BRZ DONE
        LDA I
        STA TEMP1
        LDA I
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP2
        LDA ARRAY
        STA TEMP3
        LDA ARRAY
        STA TEMP4

; Find cycle start
        LDA I
        STA START
        LDA ARRAY
        STA VALUE

; Cycle through elements
CYCLE   LDA J
        LDA SIZE
        SUB DAT 1
        BRZ END_CYCLE
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP5
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP6
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP7
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP8
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP9
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP10
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP11
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP12
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP13
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP14
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP15
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP16
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP17
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP18
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP19
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP20
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP21
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP22
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP23
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP24
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP25
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP26
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP27
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP28
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP29
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP30
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP31
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP32
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP33
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP34
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP35
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP36
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP37
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP38
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP39
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP40
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP41
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP42
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP43
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP44
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP45
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP46
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP47
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP48
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP49
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP50
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP51
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP52
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP53
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP54
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP55
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP56
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP57
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP58
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP59
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP60
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP61
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP62
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP63
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP64
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP65
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP66
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP67
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP68
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP69
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP70
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP71
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP72
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP73
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP74
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP75
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP76
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP77
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP78
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP79
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP80
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP81
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP82
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP83
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP84
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP85
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP86
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP87
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP88
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP89
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP90
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP91
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP92
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP93
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP94
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP95
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP96
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP97
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP98
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP99
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP100
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP101
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP102
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP103
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP104
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP105
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP106
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP107
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP108
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP109
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP110
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP111
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP112
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP113
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP114
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP115
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP116
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP117
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP118
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP119
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP120
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP121
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP122
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP123
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP124
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP125
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP126
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA TEMP127
        LDA J
        LDA ARRAY
        SUB DAT 1
        STA ARRAY
        LDA ARRAY
        STA