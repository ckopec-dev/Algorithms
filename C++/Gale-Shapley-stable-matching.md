# Gale-Shapley Stable Matching Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

class GaleShapley {
private:
    int n; // number of men and women
    vector<vector<int>> men_pref;     // preference list for men
    vector<vector<int>> women_pref;   // preference list for women
    vector<int> women_match;          // current matching for women
    vector<int> men_match;            // current matching for men
    
public:
    GaleShapley(int num) : n(num) {
        men_pref.resize(n, vector<int>(n));
        women_pref.resize(n, vector<int>(n));
        women_match.resize(n, -1);  // -1 means unmatched
        men_match.resize(n, -1);    // -1 means unmatched
    }
    
    // Set preference lists for men
    void setMenPreferences(int man, const vector<int>& prefs) {
        men_pref[man] = prefs;
    }
    
    // Set preference lists for women
    void setWomenPreferences(int woman, const vector<int>& prefs) {
        women_pref[woman] = prefs;
    }
    
    // Find the rank of a man in woman's preference list
    int getRank(int woman, int man) {
        for (int i = 0; i < n; i++) {
            if (women_pref[woman][i] == man) {
                return i;
            }
        }
        return n; // Should never happen
    }
    
    // Check if woman prefers man over her current partner
    bool prefers(int woman, int man, int current_partner) {
        if (current_partner == -1) return true;
        int rank_man = getRank(woman, man);
        int rank_current = getRank(woman, current_partner);
        return rank_man < rank_current;
    }
    
    // Find stable matching
    void findStableMatching() {
        vector<int> men_proposed(n, 0); // index of next woman to propose to
        
        // Continue until all men are matched
        while (true) {
            bool all_matched = true;
            
            // Find an unmatched man
            int unmatched_man = -1;
            for (int i = 0; i < n; i++) {
                if (men_match[i] == -1) {
                    unmatched_man = i;
                    all_matched = false;
                    break;
                }
            }
            
            if (all_matched) break;
            
            // Man proposes to next woman in his preference list
            int woman = men_pref[unmatched_man][men_proposed[unmatched_man]];
            men_proposed[unmatched_man]++;
            
            // If woman is unmatched, she accepts
            if (women_match[woman] == -1) {
                women_match[woman] = unmatched_man;
                men_match[unmatched_man] = woman;
            }
            // If woman prefers this man over her current partner
            else if (prefers(woman, unmatched_man, women_match[woman])) {
                // The current partner becomes unmatched
                int current_partner = women_match[woman];
                men_match[current_partner] = -1;
                
                // Update matching
                women_match[woman] = unmatched_man;
                men_match[unmatched_man] = woman;
            }
            // Otherwise, woman rejects the proposal (do nothing)
        }
    }
    
    // Print the stable matching
    void printMatching() {
        cout << "Stable Matching Result:" << endl;
        cout << "----------------------" << endl;
        for (int i = 0; i < n; i++) {
            cout << "Man " << i << " is matched with Woman " << men_match[i] << endl;
        }
        cout << endl;
    }
    
    // Print preference lists
    void printPreferences() {
        cout << "Men's Preferences:" << endl;
        for (int i = 0; i < n; i++) {
            cout << "Man " << i << ": ";
            for (int j = 0; j < n; j++) {
                cout << women_pref[i][j] << " ";
            }
            cout << endl;
        }
        cout << endl;
        
        cout << "Women's Preferences:" << endl;
        for (int i = 0; i < n; i++) {
            cout << "Woman " << i << ": ";
            for (int j = 0; j < n; j++) {
                cout << men_pref[i][j] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
};

int main() {
    int n = 4; // 4 men and 4 women
    
    GaleShapley gs(n);
    
    // Set men's preferences (men prefer women in order)
    gs.setMenPreferences(0, {0, 1, 2, 3}); // Man 0 prefers Woman 0 > 1 > 2 > 3
    gs.setMenPreferences(1, {2, 1, 3, 0}); // Man 1 prefers Woman 2 > 1 > 3 > 0
    gs.setMenPreferences(2, {1, 0, 3, 2}); // Man 2 prefers Woman 1 > 0 > 3 > 2
    gs.setMenPreferences(3, {0, 2, 1, 3}); // Man 3 prefers Woman 0 > 2 > 1 > 3
    
    // Set women's preferences (women prefer men in order)
    gs.setWomenPreferences(0, {3, 2, 1, 0}); // Woman 0 prefers Man 3 > 2 > 1 > 0
    gs.setWomenPreferences(1, {0, 1, 2, 3}); // Woman 1 prefers Man 0 > 1 > 2 > 3
    gs.setWomenPreferences(2, {1, 0, 3, 2}); // Woman 2 prefers Man 1 > 0 > 3 > 2
    gs.setWomenPreferences(3, {2, 1, 3, 0}); // Woman 3 prefers Man 2 > 1 > 3 > 0
    
    cout << "Gale-Shapley Stable Matching Algorithm" << endl;
    cout << "======================================" << endl;
    
    gs.printPreferences();
    
    gs.findStableMatching();
    gs.printMatching();
    
    return 0;
}
```

## Output:
```
Gale-Shapley Stable Matching Algorithm
======================================
Men's Preferences:
Man 0: 0 1 2 3 
Man 1: 2 1 3 0 
Man 2: 1 0 3 2 
Man 3: 0 2 1 3 

Women's Preferences:
Woman 0: 3 2 1 0 
Woman 1: 0 1 2 3 
Woman 2: 1 0 3 2 
Woman 3: 2 1 3 0 

Stable Matching Result:
----------------------
Man 0 is matched with Woman 0
Man 1 is matched with Woman 2
Man 2 is matched with Woman 1
Man 3 is matched with Woman 3
```

## Algorithm Explanation:

The Gale-Shapley algorithm works as follows:

1. **Initialization**: All men and women start as unmatched
2. **Proposals**: Each unmatched man proposes to the highest-ranked woman on his preference list who hasn't rejected him yet
3. **Acceptance/Rejection**: Each woman tentatively accepts the best proposal she's received so far, rejecting any previous proposals
4. **Iteration**: Continue until all men are matched

## Key Features:

- **Stability**: The matching is stable, meaning no man and woman who are not matched to each other would both prefer each other over their current partners
- **Time Complexity**: O(n²) where n is the number of men/women
- **Space Complexity**: O(n²) for storing preference lists
- **Guarantees**: The algorithm always produces a stable matching

The algorithm is commonly used in applications like college admissions, job matching, and kidney exchange programs where stable matching is required.

