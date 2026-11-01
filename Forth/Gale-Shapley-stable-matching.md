# Gale-Shapley Stable Matching Algorithm in Forth

Here's an implementation of the Gale-Shapley algorithm in Forth to solve the stable matching problem:

```forth
\ Gale-Shapley Stable Matching Algorithm in Forth

\ Constants and variables
100 CONSTANT MAX_PEOPLE
MAX_PEOPLE 2 * CONSTANT MAX_PAIRS

\ Arrays for storing preferences and matches
CREATE men_prefs MAX_PEOPLE 20 * ALLOT
CREATE women_prefs MAX_PEOPLE 20 * ALLOT
CREATE men_match MAX_PEOPLE ALLOT
CREATE women_match MAX_PEOPLE ALLOT
CREATE men_proposals MAX_PEOPLE ALLOT
CREATE engaged MAX_PEOPLE ALLOT

\ Initialize arrays
: init_arrays ( -- )
    0 DO
        0 men_match +!     \ No match initially
        0 women_match +!   \ No match initially
        0 men_proposals +! \ No proposals made
        0 engaged +!       \ Not engaged
    LOOP ;

\ Set preferences for men (example data)
: set_men_preferences ( -- )
    \ Man 0's preferences (woman 1, woman 2, woman 0)
    0 men_prefs + 1 !  \ Man 0 prefers woman 1 first
    0 men_prefs 1+ 2 ! \ Man 0 prefers woman 2 second
    0 men_prefs 2+ 0 ! \ Man 0 prefers woman 0 third
    
    \ Man 1's preferences (woman 2, woman 0, woman 1)
    1 men_prefs + 2 !  \ Man 1 prefers woman 2 first
    1 men_prefs 1+ 0 ! \ Man 1 prefers woman 0 second
    1 men_prefs 2+ 1 ! \ Man 1 prefers woman 1 third
    
    \ Man 2's preferences (woman 0, woman 1, woman 2)
    2 men_prefs + 0 !  \ Man 2 prefers woman 0 first
    2 men_prefs 1+ 1 ! \ Man 2 prefers woman 1 second
    2 men_prefs 2+ 2 ! \ Man 2 prefers woman 2 third ;

\ Set preferences for women (example data)
: set_women_preferences ( -- )
    \ Woman 0's preferences (man 2, man 1, man 0)
    0 women_prefs + 2 !  \ Woman 0 prefers man 2 first
    0 women_prefs 1+ 1 ! \ Woman 0 prefers man 1 second
    0 women_prefs 2+ 0 ! \ Woman 0 prefers man 0 third
    
    \ Woman 1's preferences (man 0, man 2, man 1)
    1 women_prefs + 0 !  \ Woman 1 prefers man 0 first
    1 women_prefs 1+ 2 ! \ Woman 1 prefers man 2 second
    1 women_prefs 2+ 1 ! \ Woman 1 prefers man 1 third
    
    \ Woman 2's preferences (man 1, man 0, man 2)
    2 women_prefs + 1 !  \ Woman 2 prefers man 1 first
    2 women_prefs 1+ 0 ! \ Woman 2 prefers man 0 second
    2 women_prefs 2+ 2 ! \ Woman 2 prefers man 2 third ;

\ Find the position of a person in another person's preference list
: find_position ( person target_person prefs -- pos )
    >R 0 DO
        R@ I + @ R> = IF
            I UNLOOP EXIT
        THEN
    LOOP
    -1 ; \ Not found

\ Check if a proposal is acceptable
: is_acceptable ( man woman -- flag )
    women_prefs + @ 0 DO
        I + @ dup 0= IF
            DROP FALSE EXIT
        THEN
        2DUP = IF
            2DROP TRUE EXIT
        THEN
        2DROP
    LOOP
    FALSE ;

\ Get the rank of a man in woman's preferences
: get_woman_rank ( woman man -- rank )
    women_prefs + 0 DO
        I + @ 2DUP = IF
            2DROP I UNLOOP EXIT
        THEN
        2DROP
    LOOP
    MAX_PEOPLE ; \ Not found, return high value

\ Get the rank of a woman in man's preferences
: get_man_rank ( man woman -- rank )
    men_prefs + 0 DO
        I + @ 2DUP = IF
            2DROP I UNLOOP EXIT
        THEN
        2DROP
    LOOP
    MAX_PEOPLE ; \ Not found, return high value

\ Main Gale-Shapley algorithm
: gale_shapley ( -- )
    init_arrays
    set_men_preferences
    set_women_preferences
    
    \ Initialize all men as free and propose to first woman
    0 DO
        0 men_proposals +!     \ Start with first proposal
    LOOP
    
    \ Continue until all men are engaged
    BEGIN
        TRUE
        MAX_PEOPLE 0 DO
            men_match I + @ 0= IF   \ If man is free
                men_proposals I + @ 1+ men_proposals I +! \ Increment proposals
                men_prefs I + @ 0= IF  \ If no more preferences, skip
                    FALSE
                THEN
                I men_proposals I + @ 1- + @  \ Get woman to propose to
                
                women_match I + @ 0= IF   \ If woman is free
                    I men_match +!         \ Engage them
                    I women_match +!       \ Engage them
                ELSE                     \ If woman is engaged
                    women_match I + @  \ Get current partner
                    I get_woman_rank  \ Get rank of proposing man
                    2DUP 1+ > IF      \ If proposing man is preferred
                        men_match I +!  \ Engage new man
                        women_match I +!  \ Update woman's match
                    THEN
                THEN
            THEN
        LOOP
        MAX_PEOPLE 0 DO
            men_match I + @ 0= IF
                TRUE FALSE
            THEN
        LOOP
    UNTIL
    
    \ Print results
    CR ." Stable Matching Results:" CR
    0 DO
        I men_match + @ 2DUP 0= IF
            DROP ." Man " I . ." is unmatched" CR
        ELSE
            ." Man " I . ." matches with Woman " 2DUP . CR
        THEN
    LOOP ;

\ Run the algorithm
gale_shapley
```

## How it works:

1. **Data Structures**: The implementation uses arrays to store:
   - Preference lists for men and women
   - Current matching status of people
   - Proposal tracking

2. **Key Functions**:
   - `init_arrays`: Initialize all data structures
   - `set_men_preferences`/`set_women_preferences`: Set up example preference lists
   - `gale_shapley`: Main algorithm that runs the stable matching process

3. **Algorithm Steps**:
   - All men start as free (unengaged)
   - Each free man proposes to the highest-ranked woman he hasn't proposed to yet
   - Each woman tentatively accepts the best proposal she's received so far
   - The process continues until all men are engaged

4. **Stability Check**: The algorithm ensures no two people would prefer each other over their current partners.

This implementation demonstrates the classic stable marriage problem solution in Forth, showing how the algorithm guarantees a stable matching where no blocking pairs exist.