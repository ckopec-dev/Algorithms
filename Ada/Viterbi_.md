# Viterbi Algorithm in Ada

Here's an implementation of the Viterbi algorithm for Hidden Markov Models in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Viterbi_Algorithm is
   
   -- Constants
   Num_States : constant := 2;
   Num_Observations : constant := 3;
   
   -- Transition probabilities A[i][j] = P(state j | state i)
   type Transition_Matrix is array (1..Num_States, 1..Num_States) of Float;
   A : constant Transition_Matrix := 
     ((0.7, 0.3),  -- From state 1 to state 1,2
      (0.4, 0.6));  -- From state 2 to state 1,2
   
   -- Emission probabilities B[i][k] = P(observation k | state i)
   type Emission_Matrix is array (1..Num_States, 1..Num_Observations) of Float;
   B : constant Emission_Matrix := 
     ((0.1, 0.4, 0.5),  -- State 1 emits observations 1,2,3
      (0.6, 0.3, 0.1));  -- State 2 emits observations 1,2,3
   
   -- Initial probabilities π[i] = P(state i)
   type Initial_Probabilities is array (1..Num_States) of Float;
   π : constant Initial_Probabilities := (0.5, 0.5);
   
   -- Observations sequence
   Observations : constant array (1..4) of Integer := (1, 3, 2, 1);
   
   -- Viterbi table
   type Viterbi_Table is array (1..Num_States, 1..Observations'Length) of Float;
   type Path_Table is array (1..Num_States, 1..Observations'Length) of Integer;
   
   -- Function to get emission probability
   function Get_Emission_Prob(State : Integer; Observation : Integer) return Float is
   begin
      return B(State, Observation);
   end Get_Emission_Prob;
   
   -- Function to get transition probability
   function Get_Transition_Prob(From_State : Integer; To_State : Integer) return Float is
   begin
      return A(From_State, To_State);
   end Get_Transition_Prob;
   
   -- Function to get initial probability
   function Get_Initial_Prob(State : Integer) return Float is
   begin
      return π(State);
   end Get_Initial_Prob;
   
   -- Viterbi algorithm implementation
   procedure Viterbi(Obs : in array (1..Observations'Length) of Integer;
                     Path : out array (1..Observations'Length) of Integer) is
      
      -- Viterbi table and path table
      V : Viterbi_Table;
      Psi : Path_Table;
      
      -- Temporary variables
      max_prob : Float;
      max_state : Integer;
      
   begin
      -- Initialization step
      for i in 1..Num_States loop
         V(i, 1) := Get_Initial_Prob(i) * Get_Emission_Prob(i, Obs(1));
         Psi(i, 1) := 0;
      end loop;
      
      -- Recursion step
      for t in 2..Obs'Length loop
         for j in 1..Num_States loop
            max_prob := 0.0;
            max_state := 1;
            
            -- Find the maximum probability from all previous states
            for i in 1..Num_States loop
               declare
                  prob : Float := V(i, t-1) * Get_Transition_Prob(i, j);
               begin
                  if prob > max_prob then
                     max_prob := prob;
                     max_state := i;
                  end if;
               end;
            end loop;
            
            -- Store the maximum probability and the state that gave it
            V(j, t) := max_prob * Get_Emission_Prob(j, Obs(t));
            Psi(j, t) := max_state;
         end loop;
      end loop;
      
      -- Backtracking step
      -- Find the state with maximum probability at the last time step
      max_prob := 0.0;
      max_state := 1;
      
      for i in 1..Num_States loop
         if V(i, Obs'Length) > max_prob then
            max_prob := V(i, Obs'Length);
            max_state := i;
         end if;
      end loop;
      
      -- Backtrack to find the optimal path
      Path(Obs'Length) := max_state;
      
      for t in reverse 2..Obs'Length loop
         Path(t-1) := Psi(Path(t), t);
      end loop;
      
   end Viterbi;
   
   -- Output the results
   Path : array (1..Observations'Length) of Integer;
   
begin
   -- Print observations
   Put("Observations: ");
   for i in Observations'Range loop
      Put(Observations(i), 0, 0, 0);
      if i < Observations'Length then
         Put(" ");
      end if;
   end loop;
   New_Line;
   
   -- Run Viterbi algorithm
   Viterbi(Observations, Path);
   
   -- Print the optimal path
   Put("Optimal path: ");
   for i in Path'Range loop
      Put(Path(i), 0, 0, 0);
      if i < Path'Length then
         Put(" ");
      end if;
   end loop;
   New_Line;
   
   -- Print the probability of the optimal path
   Put("Probability of optimal path: ");
   declare
      prob : Float := 1.0;
      -- This is a simplified calculation for demonstration
   begin
      for i in 1..Path'Length loop
         prob := prob * Get_Emission_Prob(Path(i), Observations(i));
      end loop;
      Put(prob, 0, 6, 0);
      New_Line;
   end;
   
end Viterbi_Algorithm;
```

## Explanation

This Ada implementation of the Viterbi algorithm:

1. **Defines the HMM parameters**:
   - Transition matrix A: probabilities of moving between states
   - Emission matrix B: probabilities of observations given states
   - Initial probabilities π: starting probabilities for each state

2. **Implements the core Viterbi algorithm**:
   - **Initialization**: Calculate probabilities for the first observation
   - **Recursion**: For each subsequent observation, calculate the maximum probability path
   - **Backtracking**: Determine the optimal state sequence by following the stored paths

3. **Key components**:
   - `Viterbi` procedure: Main algorithm implementation
   - `Get_Emission_Prob`, `Get_Transition_Prob`, `Get_Initial_Prob`: Helper functions
   - Viterbi table `V` and path table `Psi` for dynamic programming
   - Backtracking to reconstruct the optimal path

4. **Input**: Sequence of observations `[1, 3, 2, 1]`
5. **Output**: Optimal state path and probability

The algorithm efficiently finds the most likely sequence of hidden states that produced the given sequence of observations in a Hidden Markov Model.

