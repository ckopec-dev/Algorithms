# Gale-Shapley Stable Matching Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_N 100

// Structure to represent a person
typedef struct {
    int id;
    int engaged;
    int prefer[2*MAX_N];
    int current_proposal;
} Person;

// Global arrays for men and women
Person men[MAX_N];
Person women[MAX_N];

int n; // Number of men/women

// Function to initialize the algorithm
void initialize(int num) {
    n = num;
    
    // Initialize men
    for (int i = 0; i < n; i++) {
        men[i].id = i;
        men[i].engaged = -1;
        men[i].current_proposal = 0;
        for (int j = 0; j < 2*n; j++) {
            men[i].prefer[j] = -1;
        }
    }
    
    // Initialize women
    for (int i = 0; i < n; i++) {
        women[i].id = i;
        women[i].engaged = -1;
        women[i].current_proposal = 0;
        for (int j = 0; j < 2*n; j++) {
            women[i].prefer[j] = -1;
        }
    }
}

// Function to set preferences for men
void set_man_preferences(int man_id, int *preferences) {
    for (int i = 0; i < n; i++) {
        men[man_id].prefer[i] = preferences[i];
    }
}

// Function to set preferences for women
void set_woman_preferences(int woman_id, int *preferences) {
    for (int i = 0; i < n; i++) {
        women[woman_id].prefer[i] = preferences[i];
    }
}

// Function to check if a woman prefers man2 over man1
int woman_prefers_man2_over_man1(int woman_id, int man1_id, int man2_id) {
    // Find positions of both men in woman's preference list
    int pos1 = -1, pos2 = -1;
    
    for (int i = 0; i < n; i++) {
        if (women[woman_id].prefer[i] == man1_id) {
            pos1 = i;
        }
        if (women[woman_id].prefer[i] == man2_id) {
            pos2 = i;
        }
    }
    
    // If woman prefers man2 over man1
    return (pos2 < pos1);
}

// Function to perform stable matching
void gale_shapley() {
    int free_men = n;
    
    while (free_men > 0) {
        // Find a free man
        int free_man = -1;
        for (int i = 0; i < n; i++) {
            if (men[i].engaged == -1) {
                free_man = i;
                break;
            }
        }
        
        // If no free man, we're done
        if (free_man == -1) break;
        
        // Get the next woman this man will propose to
        int woman_id = men[free_man].prefer[men[free_man].current_proposal];
        men[free_man].current_proposal++;
        
        // If woman is free, engage them
        if (women[woman_id].engaged == -1) {
            women[woman_id].engaged = free_man;
            men[free_man].engaged = woman_id;
            free_men--;
        } else {
            // Woman is currently engaged to someone
            int current_partner = women[woman_id].engaged;
            
            // If woman prefers new man over current partner
            if (woman_prefers_man2_over_man1(woman_id, current_partner, free_man)) {
                // Break the current engagement
                men[current_partner].engaged = -1;
                // Engage woman with new man
                women[woman_id].engaged = free_man;
                men[free_man].engaged = woman_id;
            }
            // If woman doesn't prefer new man, she stays with current partner
        }
    }
}

// Function to print the matching result
void print_matching() {
    printf("\nStable Matching Results:\n");
    printf("------------------------\n");
    
    for (int i = 0; i < n; i++) {
        if (men[i].engaged != -1) {
            printf("Man %d is matched with Woman %d\n", i, men[i].engaged);
        }
    }
}

// Function to print preferences
void print_preferences() {
    printf("\nMen's Preferences:\n");
    for (int i = 0; i < n; i++) {
        printf("Man %d: ", i);
        for (int j = 0; j < n; j++) {
            printf("%d ", men[i].prefer[j]);
        }
        printf("\n");
    }
    
    printf("\nWomen's Preferences:\n");
    for (int i = 0; i < n; i++) {
        printf("Woman %d: ", i);
        for (int j = 0; j < n; j++) {
            printf("%d ", women[i].prefer[j]);
        }
        printf("\n");
    }
}

// Example usage
int main() {
    // Initialize with 4 men and 4 women
    initialize(4);
    
    // Set preferences for men
    int man0_pref[] = {0, 1, 2, 3};  // Man 0 prefers: Woman 0, 1, 2, 3
    int man1_pref[] = {1, 0, 2, 3};  // Man 1 prefers: Woman 1, 0, 2, 3
    int man2_pref[] = {2, 1, 0, 3};  // Man 2 prefers: Woman 2, 1, 0, 3
    int man3_pref[] = {3, 2, 1, 0};  // Man 3 prefers: Woman 3, 2, 1, 0
    
    set_man_preferences(0, man0_pref);
    set_man_preferences(1, man1_pref);
    set_man_preferences(2, man2_pref);
    set_man_preferences(3, man3_pref);
    
    // Set preferences for women
    int woman0_pref[] = {3, 2, 1, 0};  // Woman 0 prefers: Man 3, 2, 1, 0
    int woman1_pref[] = {2, 3, 1, 0};  // Woman 1 prefers: Man 2, 3, 1, 0
    int woman2_pref[] = {1, 0, 3, 2};  // Woman 2 prefers: Man 1, 0, 3, 2
    int woman3_pref[] = {0, 1, 2, 3};  // Woman 3 prefers: Man 0, 1, 2, 3
    
    set_woman_preferences(0, woman0_pref);
    set_woman_preferences(1, woman1_pref);
    set_woman_preferences(2, woman2_pref);
    set_woman_preferences(3, woman3_pref);
    
    printf("Gale-Shapley Stable Matching Algorithm\n");
    printf("=====================================\n");
    
    // Print preferences
    print_preferences();
    
    // Run the algorithm
    gale_shapley();
    
    // Print results
    print_matching();
    
    return 0;
}
```

## How it works:

1. **Initialization**: Sets up arrays to represent men and women with their preferences
2. **Preference Setting**: Men and women each have ranked all members of the opposite sex
3. **Proposal Process**: 
   - Each free man proposes to the highest-ranked woman he hasn't proposed to yet
   - If a woman is free, she accepts the proposal
   - If a woman is engaged, she compares her current partner with the new proposer and accepts the better match
4. **Result**: A stable matching where no man and woman who are not matched to each other both prefer each other over their current partners

## Sample Output:
```
Gale-Shapley Stable Matching Algorithm
=====================================

Men's Preferences:
Man 0: 0 1 2 3 
Man 1: 1 0 2 3 
Man 2: 2 1 0 3 
Man 3: 3 2 1 0 

Women's Preferences:
Woman 0: 3 2 1 0 
Woman 1: 2 3 1 0 
Woman 2: 1 0 3 2 
Woman 3: 0 1 2 3 

Stable Matching Results:
------------------------
Man 0 is matched with Woman 0
Man 1 is matched with Woman 1
Man 2 is matched with Woman 2
Man 3 is matched with Woman 3
```

This implementation demonstrates the classic stable marriage problem solution where each person has a complete ranking of preferences for the opposite sex.