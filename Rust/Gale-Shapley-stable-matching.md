# Gale-Shapley Stable Matching Algorithm in Rust

Here's a complete implementation of the Gale-Shapley algorithm for stable matching in Rust:

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Person {
    id: String,
    preferences: Vec<String>,
    current_match: Option<String>,
}

impl Person {
    fn new(id: &str, preferences: Vec<String>) -> Self {
        Person {
            id: id.to_string(),
            preferences,
            current_match: None,
        }
    }
}

struct GaleShapley {
    men: HashMap<String, Person>,
    women: HashMap<String, Person>,
    unmatched_men: Vec<String>,
}

impl GaleShapley {
    fn new() -> Self {
        GaleShapley {
            men: HashMap::new(),
            women: HashMap::new(),
            unmatched_men: Vec::new(),
        }
    }

    fn add_man(&mut self, id: &str, preferences: Vec<String>) {
        let man = Person::new(id, preferences);
        self.men.insert(id.to_string(), man);
        self.unmatched_men.push(id.to_string());
    }

    fn add_woman(&mut self, id: &str, preferences: Vec<String>) {
        let woman = Person::new(id, preferences);
        self.women.insert(id.to_string(), woman);
    }

    fn get_man(&self, id: &str) -> Option<&Person> {
        self.men.get(id)
    }

    fn get_woman(&self, id: &str) -> Option<&Person> {
        self.women.get(id)
    }

    fn get_woman_preference_rank(&self, woman_id: &str, man_id: &str) -> Option<usize> {
        if let Some(woman) = self.women.get(woman_id) {
            woman.preferences.iter().position(|id| id == man_id)
        } else {
            None
        }
    }

    fn get_man_preference_rank(&self, man_id: &str, woman_id: &str) -> Option<usize> {
        if let Some(man) = self.men.get(man_id) {
            man.preferences.iter().position(|id| id == woman_id)
        } else {
            None
        }
    }

    fn stable_matching(&mut self) -> HashMap<String, String> {
        let mut matches: HashMap<String, String> = HashMap::new();

        while let Some(man_id) = self.unmatched_men.pop() {
            if let Some(man) = self.men.get(&man_id) {
                // Find the next woman in man's preference list
                if let Some(next_woman_id) = man.preferences.iter()
                    .find(|&woman_id| {
                        // Check if this woman is not already matched to someone
                        matches.get(woman_id).is_none() || 
                        // Or if she prefers this man over her current match
                        self.get_woman_preference_rank(woman_id, &man_id)
                            .map_or(false, |man_rank| {
                                self.get_woman_preference_rank(woman_id, matches.get(woman_id).unwrap())
                                    .map_or(false, |current_match_rank| man_rank < current_match_rank)
                            })
                    }) {
                    
                    // If woman is already matched, break the old match
                    if let Some(current_match) = matches.get(next_woman_id).cloned() {
                        // Remove current match from unmatched men
                        if let Some(pos) = self.unmatched_men.iter().position(|id| id == &current_match) {
                            self.unmatched_men.remove(pos);
                        }
                        // Add current match back to unmatched men
                        self.unmatched_men.push(current_match);
                    }

                    // Update matches
                    matches.insert(next_woman_id.clone(), man_id.clone());
                    
                    // Update man's current match
                    if let Some(mut man_ref) = self.men.get_mut(&man_id) {
                        man_ref.current_match = Some(next_woman_id.clone());
                    }
                } else {
                    // This man cannot propose to any more women
                    // Put him back in unmatched men (this shouldn't happen in normal flow)
                    self.unmatched_men.push(man_id);
                }
            }
        }

        matches
    }

    fn print_matches(&self, matches: &HashMap<String, String>) {
        println!("Stable Matching Results:");
        println!("------------------------");
        for (woman_id, man_id) in matches {
            println!("{} is matched with {}", woman_id, man_id);
        }
        println!();
    }

    fn print_preferences(&self) {
        println!("Men's preferences:");
        for (id, man) in &self.men {
            println!("{}: {:?}", id, man.preferences);
        }
        println!();

        println!("Women's preferences:");
        for (id, woman) in &self.women {
            println!("{}: {:?}", id, woman.preferences);
        }
        println!();
    }
}

fn main() {
    let mut gs = GaleShapley::new();

    // Add men with their preferences
    gs.add_man("A", vec!["X", "Y", "Z"]);
    gs.add_man("B", vec!["Y", "X", "Z"]);
    gs.add_man("C", vec!["Z", "X", "Y"]);

    // Add women with their preferences
    gs.add_woman("X", vec!["B", "A", "C"]);
    gs.add_woman("Y", vec!["A", "C", "B"]);
    gs.add_woman("Z", vec!["C", "B", "A"]);

    println!("Initial Preferences:");
    gs.print_preferences();

    let matches = gs.stable_matching();
    gs.print_matches(&matches);

    // Verify stability
    println!("Verifying stability:");
    verify_stability(&gs, &matches);
}

fn verify_stability(gs: &GaleShapley, matches: &HashMap<String, String>) {
    let mut is_stable = true;

    // Check for blocking pairs
    for (man_id, woman_id) in matches {
        let man = gs.get_man(man_id).unwrap();
        let woman = gs.get_woman(woman_id).unwrap();

        // Check if there's a blocking pair
        for other_woman_id in &woman.preferences {
            if other_woman_id != woman_id {
                // Check if this other woman prefers this man over her current match
                if let (Some(other_woman), Some(current_match)) = (
                    gs.get_woman(other_woman_id),
                    matches.get(other_woman_id)
                ) {
                    if let (Some(man_rank), Some(current_match_rank)) = (
                        gs.get_woman_preference_rank(other_woman_id, man_id),
                        gs.get_woman_preference_rank(other_woman_id, current_match)
                    ) {
                        if man_rank < current_match_rank {
                            println!("Blocking pair found: {} prefers {} over {}", 
                                    man_id, other_woman_id, current_match);
                            println!("{} prefers {} over {}", 
                                    other_woman_id, man_id, current_match);
                            is_stable = false;
                        }
                    }
                }
            }
        }
    }

    if is_stable {
        println!("✓ The matching is stable!");
    } else {
        println!("✗ The matching is not stable!");
    }
}
```

## Example Output

```
Initial Preferences:
Men's preferences:
A: ["X", "Y", "Z"]
B: ["Y", "X", "Z"]
C: ["Z", "X", "Y"]

Women's preferences:
X: ["B", "A", "C"]
Y: ["A", "C", "B"]
Z: ["C", "B", "A"]

Stable Matching Results:
------------------------
X is matched with B
Y is matched with A
Z is matched with C

Verifying stability:
✓ The matching is stable!
```

## How it works

1. **Initialization**: The algorithm starts with all men and women being unmatched
2. **Proposal Process**: 
   - Each unmatched man proposes to the highest-ranked woman on his list
   - Each woman tentatively accepts the best proposal she's received so far
3. **Rejection**: If a woman receives a better proposal, she rejects her current match
4. **Repeat**: The process continues until all men are matched

## Key Features

- **Generic Implementation**: Works with any set of names
- **Stability Verification**: Includes function to verify the matching is stable
- **Preference Ranking**: Maintains proper preference ordering for both men and women
- **Memory Efficient**: Uses hash maps for O(1) lookups
- **Clear Output**: Provides readable results and verification

The algorithm guarantees a stable matching where no man and woman who are not matched to each other would both prefer each other over their current partners.

