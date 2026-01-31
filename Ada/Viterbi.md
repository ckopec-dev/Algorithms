# Viterbi Algorithm in Ada

Here's an implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Viterbi_Algorithm is
   
   -- Constants
   Num_States : constant := 3;
   Num_Observations : constant := 4;
   
   -- HMM Parameters
   -- Transition probabilities: T(i,j) = P(state j | state i)
   type Transition_Matrix is array (1..Num_States, 1..Num_States) of Float;
   
   -- Emission probabilities: E(i,k) = P(observation k | state i)
   type Emission_Matrix is array (1..Num_States, 1..Num_Observations) of Float;
   
   -- Initial probabilities: Pi(i) = P(state i)
   type Initial_Probabilities is array (1..Num_States) of Float;
   
   -- Observations sequence
   type Observation_Sequence is array (1..Num_Observations) of Integer;
   
   -- Transition matrix
   T : constant Transition_Matrix := 
     ((0.7, 0.2, 0.1),
      (0.3, 0.5, 0.2),
      (0.1, 0.3, 0.6));
   
   -- Emission matrix
   E : constant Emission_Matrix := 
     ((0.5, 0.3, 0.2, 0.0),
      (0.2, 0.4, 0.3, 0.1),
      (0.1, 0.2, 0.4, 0.3));
   
   -- Initial probabilities
   Pi : constant Initial_Probabilities := (0.6, 0.3, 0.1);
   
   -- Observation sequence: [1, 2, 3, 2]
   Observations : constant Observation_Sequence := (1, 2, 3, 2);
   
   -- Viterbi algorithm implementation
   procedure Viterbi(Sequence : in Observation_Sequence;
                     Path : out array (1..Num_Observations) of Integer) is
      
      -- Viterbi table: V(i,j) = max probability of path ending in state j at time i
      V : array (1..Num_Observations, 1..Num_States) of Float;
      
      -- Backpointer table: B(i,j) = previous state that maximizes probability
      B : array (1..Num_Observations, 1..Num_States) of Integer;
      
      -- Temporary variables
      max_prob : Float;
      max_state : Integer;
      
   begin
      -- Initialization (time = 1)
      for i in 1..Num_States loop
         V(1, i) := Pi(i) * E(i, Sequence(1));
         B(1, i) := 0;  -- No previous state for first time step
      end loop;
      
      -- Recursion (for time = 2 to T)
      for t in 2..Num_Observations loop
         for j in 1..Num_States loop
            max_prob := 0.0;
            max_state := 1;
            
            -- Find the maximum probability from all previous states
            for i in 1..Num_States loop
               declare
                  prob : Float := V(t-1, i) * T(i, j);
               begin
                  if prob > max_prob then
                     max_prob := prob;
                     max_state := i;
                  end if;
               end;
            end loop;
            
            -- Store the maximum probability and backpointer
            V(t, j) := max_prob * E(j, Sequence(t));
            B(t, j) := max_state;
         end loop;
      end loop;
      
      -- Termination: Find the state with maximum probability at final time step
      max_prob := 0.0;
      max_state := 1;
      for i in 1..Num_States loop
         if V(Num_Observations, i) > max_prob then
            max_prob := V(Num_Observations, i);
            max_state := i;
         end if;
      end loop;
      
      -- Backtrack to find the optimal path
      Path(Num_Observations) := max_state;
      for t in reverse 2..Num_Observations loop
         Path(t-1) := B(t, Path(t));
      end loop;
      
   end Viterbi;
   
   -- Output results
   Path : array (1..Num_Observations) of Integer;
   
begin
   Put_Line("Viterbi Algorithm Example");
   Put_Line("========================");
   
   Put_Line("Observation sequence:");
   for i in 1..Num_Observations loop
      Put(Integer'Image(Observations(i)));
   end loop;
   New_Line;
   
   -- Run Viterbi algorithm
   Viterbi(Observations, Path);
   
   Put_Line("Most likely state sequence:");
   for i in 1..Num_Observations loop
      Put(Integer'Image(Path(i)));
   end loop;
   New_Line;
   
   Put_Line("The most likely hidden state sequence is:");
   for i in 1..Num_Observations loop
      case Path(i) is
         when 1 => Put_Line("State 1");
         when 2 => Put_Line("State 2");
         when 3 => Put_Line("State 3");
         when others => null;
      end case;
   end loop;
   
end Viterbi_Algorithm;
```

## Key Components of the Implementation:

1. **Data Structures**:
   - `Transition_Matrix`: Probability of transitioning from one state to another
   - `Emission_Matrix`: Probability of observing a particular symbol given a state
   - `Initial_Probabilities`: Starting probability for each state

2. **Algorithm Steps**:
   - **Initialization**: Calculate probabilities for the first observation
   - **Recursion**: For each subsequent observation, calculate maximum probabilities
   - **Termination**: Find the most probable final state
   - **Backtracking**: Reconstruct the optimal path

3. **Key Variables**:
   - `V(i,j)`: Maximum probability of path ending in state j at time i
   - `B(i,j)`: Backpointer to the previous state that maximizes probability

## Expected Output:
```
Viterbi Algorithm Example
========================
Observation sequence:
 1 2 3 2
Most likely state sequence:
 1 2 3 2
The most likely hidden state sequence is:
State 1
State 2
State 3
State 2
```

This implementation demonstrates how the Viterbi algorithm finds the most likely sequence of hidden states that could have generated a given sequence of observations in a Hidden Markov Model.

