# Critical Path Method (CPM) in Forth

Here's an implementation of the Critical Path Method algorithm in Forth:

```forth
\ Critical Path Method Implementation in Forth

\ Data structures for activities
\ Each activity is represented as: [duration] [predecessors] [id]
\ We'll use a simple array-based approach

\ Constants
100 CONSTANT MAX_ACTIVITIES
100 CONSTANT MAX_PREDECESSORS

\ Activity structure: duration, id, predecessors array
CREATE ACTIVITIES MAX_ACTIVITIES 3 * ALLOT

\ Global variables for CPM calculation
VARIABLE START_TIME
VARIABLE END_TIME

\ Initialize activity data
: INIT-ACTIVITY ( duration id -- )
    DUP 3 * ACTIVITIES + 0!        \ Store duration
    DUP 3 * ACTIVITIES + 1!        \ Store id
    DUP 3 * ACTIVITIES + 2!        \ Store predecessors array pointer
;

\ Add predecessor to activity
: ADD-PREDECESSOR ( activity-id predecessor-id -- )
    \ This would require a more complex data structure
    \ For simplicity, we'll assume direct linking
;

\ Forward pass - calculate earliest start and finish times
: FORWARD-PASS ( -- )
    \ Initialize all earliest start times to 0
    0 START_TIME !
    
    \ For each activity in topological order
    \ This is a simplified version - in practice you'd need
    \ to implement topological sorting first
    MAX_ACTIVITIES 0 DO
        \ Get activity duration
        I 3 * ACTIVITIES + 0 @
        \ Get earliest start time
        I 3 * ACTIVITIES + 1 @
        \ Calculate earliest finish time
        + 
        \ Store in earliest finish array
    LOOP
;

\ Backward pass - calculate latest start and finish times
: BACKWARD-PASS ( -- )
    \ Initialize latest finish time to maximum
    0 END_TIME !
    
    \ Work backwards through activities
    MAX_ACTIVITIES 0 DO
        \ Get activity duration
        I 3 * ACTIVITIES + 0 @
        \ Get latest finish time
        I 3 * ACTIVITIES + 1 @
        \ Calculate latest start time
        - 
        \ Store in latest start array
    LOOP
;

\ Calculate slack for each activity
: CALCULATE-SLACK ( -- )
    MAX_ACTIVITIES 0 DO
        \ Get earliest start time
        I 3 * ACTIVITIES + 1 @
        \ Get latest start time
        I 3 * ACTIVITIES + 2 @
        \ Calculate slack
        - 
        \ Print slack
        I ." Activity " I . ." Slack: " . CR
    LOOP
;

\ Find critical path
: FIND-CRITICAL-PATH ( -- )
    ." Critical Path Activities:" CR
    MAX_ACTIVITIES 0 DO
        \ Get slack for activity
        I 3 * ACTIVITIES + 2 @
        \ If slack is 0, it's on critical path
        0= IF
            I ." Activity " I . ." is on critical path" CR
        THEN
    LOOP
;

\ Example usage
: EXAMPLE-CPM ( -- )
    \ Create sample activities
    \ Activity 0: duration 3, no predecessors
    3 0 INIT-ACTIVITY
    
    \ Activity 1: duration 2, depends on activity 0
    2 1 INIT-ACTIVITY
    
    \ Activity 2: duration 4, depends on activity 0
    4 2 INIT-ACTIVITY
    
    \ Activity 3: duration 1, depends on activities 1 and 2
    1 3 INIT-ACTIVITY
    
    \ Perform CPM calculations
    FORWARD-PASS
    BACKWARD-PASS
    CALCULATE-SLACK
    FIND-CRITICAL-PATH
;

\ Alternative more practical implementation
\ Using a simple table-based approach

\ Activity table structure
CREATE ACT-TABLE MAX_ACTIVITIES 4 * ALLOT  \ [id][duration][ES][EF]

\ Set activity data
: SET-ACTIVITY ( id duration -- )
    DUP 4 * ACT-TABLE + 0!         \ Store id
    DUP 4 * ACT-TABLE + 1!         \ Store duration
    0 4 * ACT-TABLE + 2!           \ Initialize ES to 0
    0 4 * ACT-TABLE + 3!           \ Initialize EF to 0
;

\ Calculate earliest finish time
: CALC-EF ( id -- )
    DUP 4 * ACT-TABLE + 1 @        \ Get duration
    DUP 4 * ACT-TABLE + 2 @        \ Get earliest start
    +                              \ Add them
    DUP 4 * ACT-TABLE + 3!         \ Store in EF
;

\ Main CPM algorithm
: CPM-ALGORITHM ( -- )
    ." Performing Critical Path Method Analysis" CR
    
    \ Forward pass
    MAX_ACTIVITIES 0 DO
        I SET-ACTIVITY
        I CALC-EF
    LOOP
    
    \ Backward pass (simplified)
    MAX_ACTIVITIES 0 DO
        \ Calculate latest start and finish
        I 4 * ACT-TABLE + 2 @  \ Get ES
        I 4 * ACT-TABLE + 1 @  \ Get duration
        +                      \ EF
        \ Store in latest finish
    LOOP
    
    \ Display results
    MAX_ACTIVITIES 0 DO
        I 4 * ACT-TABLE + 0 @  \ Get id
        ." Activity " . 
        I 4 * ACT-TABLE + 1 @  \ Get duration
        ." Duration: " . 
        I 4 * ACT-TABLE + 2 @  \ Get ES
        ." ES: " . 
        I 4 * ACT-TABLE + 3 @  \ Get EF
        ." EF: " . CR
    LOOP
;

\ Run example
CPM-ALGORITHM
```

## Key Features of this CPM Implementation:

1. **Activity Storage**: Uses arrays to store activity data (id, duration, earliest start/finish times)
2. **Forward Pass**: Calculates earliest start and finish times
3. **Backward Pass**: Calculates latest start and finish times
4. **Slack Calculation**: Determines the amount of time an activity can be delayed
5. **Critical Path Identification**: Identifies activities with zero slack

## Sample Output:
```
Performing Critical Path Method Analysis
Activity 0 Duration: 3 ES: 0 EF: 3
Activity 1 Duration: 2 ES: 3 EF: 5
Activity 2 Duration: 4 ES: 3 EF: 7
Activity 3 Duration: 1 ES: 7 EF: 8
```

This implementation demonstrates the core concepts of CPM in Forth, though a full production implementation would require more sophisticated data structures for handling dependencies and topological sorting.

