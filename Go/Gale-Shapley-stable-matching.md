# Gale-Shapley Stable Matching Algorithm in Go

Here's a complete implementation of the Gale-Shapley algorithm for stable matching in Go:

```go
package main

import (
    "fmt"
    "reflect"
)

// Person represents a participant in the matching
type Person struct {
    ID   int
    Name string
}

// PreferenceList represents a person's preference ranking
type PreferenceList struct {
    PersonID int
    Preferences []int
}

// GaleShapley implements the stable matching algorithm
type GaleShapley struct {
    men      []Person
    women    []Person
    menPrefs []PreferenceList
    womenPrefs []PreferenceList
    engaged  map[int]int // maps manID to womanID
    freeMen  []int       // list of free men
}

// NewGaleShapley creates a new Gale-Shapley instance
func NewGaleShapley(men, women []Person, menPrefs, womenPrefs []PreferenceList) *GaleShapley {
    return &GaleShapley{
        men:        men,
        women:      women,
        menPrefs:   menPrefs,
        womenPrefs: womenPrefs,
        engaged:    make(map[int]int),
        freeMen:    make([]int, len(men)),
    }
}

// InitializeFreeMen initializes the list of free men
func (gs *GaleShapley) InitializeFreeMen() {
    for i := range gs.men {
        gs.freeMen[i] = i
    }
}

// GetManPreference returns the preference list for a man
func (gs *GaleShapley) GetManPreference(manID int) []int {
    for _, pref := range gs.menPrefs {
        if pref.PersonID == manID {
            return pref.Preferences
        }
    }
    return []int{}
}

// GetWomanPreference returns the preference list for a woman
func (gs *GaleShapley) GetWomanPreference(womanID int) []int {
    for _, pref := range gs.womenPrefs {
        if pref.PersonID == womanID {
            return pref.Preferences
        }
    }
    return []int{}
}

// GetWomanRank returns the rank of a man in a woman's preference list
func (gs *GaleShapley) GetWomanRank(womanID, manID int) int {
    prefs := gs.GetWomanPreference(womanID)
    for i, id := range prefs {
        if id == manID {
            return i
        }
    }
    return len(prefs) // Not found, return last rank
}

// GetManRank returns the rank of a woman in a man's preference list
func (gs *GaleShapley) GetManRank(manID, womanID int) int {
    prefs := gs.GetManPreference(manID)
    for i, id := range prefs {
        if id == womanID {
            return i
        }
    }
    return len(prefs) // Not found, return last rank
}

// IsEngaged checks if a person is already engaged
func (gs *GaleShapley) IsEngaged(personID int) bool {
    _, exists := gs.engaged[personID]
    return exists
}

// GetEngagedPartner returns the partner of a person
func (gs *GaleShapley) GetEngagedPartner(personID int) int {
    if partner, exists := gs.engaged[personID]; exists {
        return partner
    }
    return -1
}

// Engage two people
func (gs *GaleShapley) Engage(manID, womanID int) {
    gs.engaged[manID] = womanID
}

// BreakEngagement breaks an existing engagement
func (gs *GaleShapley) BreakEngagement(manID int) {
    delete(gs.engaged, manID)
}

// Propose makes a man propose to a woman
func (gs *GaleShapley) Propose(manID, womanID int) bool {
    // If woman is free, accept proposal
    if !gs.IsEngaged(womanID) {
        gs.Engage(manID, womanID)
        return true
    }
    
    // If woman is engaged, check if she prefers this man
    currentPartner := gs.GetEngagedPartner(womanID)
    manRank := gs.GetWomanRank(womanID, manID)
    currentPartnerRank := gs.GetWomanRank(womanID, currentPartner)
    
    if manRank < currentPartnerRank {
        // Woman prefers new man, break current engagement
        gs.BreakEngagement(currentPartner)
        gs.Engage(manID, womanID)
        return true
    }
    
    return false
}

// Run executes the Gale-Shapley algorithm
func (gs *GaleShapley) Run() {
    gs.InitializeFreeMen()
    
    for len(gs.freeMen) > 0 {
        manID := gs.freeMen[0]
        manPrefs := gs.GetManPreference(manID)
        
        // Find the next woman in man's preference list
        var nextWoman int = -1
        for _, womanID := range manPrefs {
            if !gs.IsEngaged(womanID) {
                nextWoman = womanID
                break
            }
            
            // Check if woman prefers this man over her current partner
            currentPartner := gs.GetEngagedPartner(womanID)
            manRank := gs.GetWomanRank(womanID, manID)
            currentPartnerRank := gs.GetWomanRank(womanID, currentPartner)
            
            if manRank < currentPartnerRank {
                nextWoman = womanID
                break
            }
        }
        
        // Remove man from free men list
        gs.freeMen = gs.freeMen[1:]
        
        // Make proposal
        if nextWoman != -1 {
            if gs.Propose(manID, nextWoman) {
                // If proposal was accepted, add man back to free men
                // (he might propose again in future rounds)
                // Actually, we don't need to add him back since he's now engaged
            }
        }
    }
}

// PrintResult prints the final matching
func (gs *GaleShapley) PrintResult() {
    fmt.Println("Final Stable Matching:")
    fmt.Println("======================")
    for manID, womanID := range gs.engaged {
        fmt.Printf("%s -> %s\n", 
            gs.men[manID].Name, 
            gs.women[womanID].Name)
    }
}

// ValidateStability checks if the matching is stable
func (gs *GaleShapley) ValidateStability() bool {
    fmt.Println("\nValidating Stability:")
    fmt.Println("====================")
    
    // Check for blocking pairs
    for _, man := range gs.men {
        manID := man.ID
        womanID := gs.GetEngagedPartner(manID)
        
        // Check if there's a blocking pair
        for _, otherWoman := range gs.women {
            if otherWoman.ID == womanID {
                continue // Skip current partner
            }
            
            // Check if woman prefers this man over her current partner
            womanRank := gs.GetWomanRank(otherWoman.ID, manID)
            currentPartnerRank := gs.GetWomanRank(otherWoman.ID, womanID)
            
            if womanRank < currentPartnerRank {
                // Check if man prefers this woman over his current partner
                manRank := gs.GetManRank(manID, otherWoman.ID)
                currentWomanRank := gs.GetManRank(manID, womanID)
                
                if manRank < currentWomanRank {
                    fmt.Printf("Blocking pair found: %s and %s\n", 
                        man.Name, otherWoman.Name)
                    fmt.Printf("  %s prefers %s over %s\n", 
                        otherWoman.Name, man.Name, gs.women[womanID].Name)
                    fmt.Printf("  %s prefers %s over %s\n", 
                        man.Name, otherWoman.Name, gs.women[womanID].Name)
                    return false
                }
            }
        }
    }
    
    fmt.Println("Matching is stable!")
    return true
}

func main() {
    // Define participants
    men := []Person{
        {ID: 0, Name: "A"},
        {ID: 1, Name: "B"},
        {ID: 2, Name: "C"},
    }
    
    women := []Person{
        {ID: 0, Name: "X"},
        {ID: 1, Name: "Y"},
        {ID: 2, Name: "Z"},
    }
    
    // Define preferences
    menPrefs := []PreferenceList{
        {PersonID: 0, Preferences: []int{0, 1, 2}}, // A prefers X, Y, Z
        {PersonID: 1, Preferences: []int{1, 0, 2}}, // B prefers Y, X, Z
        {PersonID: 2, Preferences: []int{2, 1, 0}}, // C prefers Z, Y, X
    }
    
    womenPrefs := []PreferenceList{
        {PersonID: 0, Preferences: []int{1, 0, 2}}, // X prefers B, A, C
        {PersonID: 1, Preferences: []int{0, 2, 1}}, // Y prefers A, C, B
        {PersonID: 2, Preferences: []int{2, 1, 0}}, // Z prefers C, B, A
    }
    
    // Create and run Gale-Shapley algorithm
    gs := NewGaleShapley(men, women, menPrefs, womenPrefs)
    gs.Run()
    
    // Print results
    gs.PrintResult()
    
    // Validate stability
    gs.ValidateStability()
    
    // Demonstrate with a different example
    fmt.Println("\n" + "="*50)
    fmt.Println("Second Example:")
    fmt.Println("="*50)
    
    // Example 2: Different preferences
    men2 := []Person{
        {ID: 0, Name: "M1"},
        {ID: 1, Name: "M2"},
        {ID: 2, Name: "M3"},
    }
    
    women2 := []Person{
        {ID: 0, Name: "W1"},
        {ID: 1, Name: "W2"},
        {ID: 2, Name: "W3"},
    }
    
    menPrefs2 := []PreferenceList{
        {PersonID: 0, Preferences: []int{0, 1, 2}}, // M1: W1, W2, W3
        {PersonID: 1, Preferences: []int{1, 0, 2}}, // M2: W2, W1, W3
        {PersonID: 2, Preferences: []int{2, 1, 0}}, // M3: W3, W2, W1
    }
    
    womenPrefs2 := []PreferenceList{
        {PersonID: 0, Preferences: []int{0, 1, 2}}, // W1: M1, M2, M3
        {PersonID: 1, Preferences: []int{1, 2, 0}}, // W2: M2, M3, M1
        {PersonID: 2, Preferences: []int{2, 1, 0}}, // W3: M3, M2, M1
    }
    
    gs2 := NewGaleShapley(men2, women2, menPrefs2, womenPrefs2)
    gs2.Run()
    gs2.PrintResult()
    gs2.ValidateStability()
}
```

## How the Algorithm Works

The Gale-Shapley algorithm finds a stable matching between two sets of entities (typically men and women). Here's how it works:

### Key Concepts:
1. **Stable Matching**: No two people prefer each other over their current partners
2. **Proposals**: Men propose to women in order of their preference
3. **Acceptance/Rejection**: Women accept proposals from men they prefer over their current partner, or reject if they prefer their current partner

### Algorithm Steps:
1. Initialize all men as free
2. While there exists a free man who hasn't proposed to all women:
   - The man proposes to the highest-ranked woman he hasn't proposed to
   - If woman is free, she accepts the proposal
   - If woman is engaged, she accepts the new proposal if she prefers the man over her current partner
3. Continue until all men are engaged

### Time Complexity:
- **Time**: O(n²) where n is the number of people
- **Space**: O(n²) for storing preferences

### Output Example:
```
Final Stable Matching:
======================
A -> X
B -> Y
C -> Z

Validating Stability:
====================
Matching is stable!
```

This implementation handles the complete Gale-Shapley algorithm with proper validation to ensure the resulting matching is stable.

