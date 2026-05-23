# Jarvis March Algorithm in LMC

Here's an implementation of the Jarvis March (Gift Wrapping) algorithm in LMC (Little Man Computer) assembly language:

```assembly
        INP
        STA POINTS
        INP
        STA POINTS+1
        INP
        STA POINTS+2
        INP
        STA POINTS+3
        INP
        STA POINTS+4
        INP
        STA POINTS+5
        INP
        STA POINTS+6
        INP
        STA POINTS+7
        INP
        STA POINTS+8
        INP
        STA POINTS+9
        INP
        STA POINTS+10
        INP
        STA POINTS+11

        LDA POINTS
        STA MIN_X
        LDA POINTS+1
        STA MIN_Y
        LDA POINTS
        STA START_X
        LDA POINTS+1
        STA START_Y
        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS
        STA CURRENT_X
        LDA POINTS+1
        STA CURRENT_Y
        LDA POINTS+2
        STA NEXT_X
        LDA POINTS+3
        STA NEXT_Y

        LDA POINTS+2
        STA ANGLE
        LDA POINTS+3
        STA ANGLE+1
        LDA POINTS+4
        STA ANGLE+2
        LDA POINTS+5
        STA ANGLE+3
        LDA POINTS+6
        STA ANGLE+4
        LDA POINTS+7
        STA ANGLE+5
        LDA POINTS+8
        STA ANGLE+6
        LDA POINTS+9
        STA ANGLE+7
        LDA POINTS+10
        STA ANGLE+8
        LDA POINTS+11
        STA ANGLE+9

        LDA POINTS
        STA LAST_X
        LDA POINTS+1
        STA LAST_Y

        LDA POINTS+2
        STA NEXT_X
        LDA POINTS+3
        STA NEXT_Y

        LDA POINTS+4
        STA TEMP_X
        LDA POINTS+5
        STA TEMP_Y

        LDA POINTS+6
        STA ANGLE_X
        LDA POINTS+7
        STA ANGLE_Y

        LDA POINTS+8
        STA ANGLE_X+1
        LDA POINTS+9
        STA ANGLE_Y+1

        LDA POINTS+10
        STA ANGLE_X+2
        LDA POINTS+11
        STA ANGLE_Y+2

        LDA POINTS+2
        STA TEMP_X
        LDA POINTS+3
        STA TEMP_Y

        LDA POINTS+4
        STA ANGLE_X
        LDA POINTS+5
        STA ANGLE_Y

        LDA POINTS+6
        STA ANGLE_X+1
        LDA POINTS+7
        STA ANGLE_Y+1

        LDA POINTS+8
        STA ANGLE_X+2
        LDA POINTS+9
        STA ANGLE_Y+2

        LDA POINTS+10
        STA ANGLE_X+3
        LDA POINTS+11
        STA ANGLE_Y+3

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINTS+7
        STA HULL+7

        LDA POINTS+8
        STA HULL+8
        LDA POINTS+9
        STA HULL+9

        LDA POINTS+10
        STA HULL+10
        LDA POINTS+11
        STA HULL+11

        LDA POINTS
        STA HULL
        LDA POINTS+1
        STA HULL+1

        LDA POINTS+2
        STA HULL+2
        LDA POINTS+3
        STA HULL+3

        LDA POINTS+4
        STA HULL+4
        LDA POINTS+5
        STA HULL+5

        LDA POINTS+6
        STA HULL+6
        LDA POINT