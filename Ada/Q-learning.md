# Q-Learning Algorithm in Ada

Here's an implementation of a Q-learning algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Command_Line; use Ada.Command_Line;

procedure Q_Learning is
   
   -- Q-Learning parameters
   type State is range 0 .. 3;
   type Action is range 0 .. 3;
   
   -- Q-table dimensions
   Num_States : constant := 4;
   Num_Actions : constant := 4;
   
   -- Q-table type
   type Q_Table is array (State, Action) of Float;
   
   -- Learning parameters
   Alpha : constant Float := 0.1;     -- Learning rate
   Gamma : constant Float := 0.9;     -- Discount factor
   Epsilon : constant Float := 0.1;   -- Exploration rate
   
   -- Reward matrix (state, action) -> reward
   type Reward_Matrix is array (State, Action) of Float;
   
   -- Q-table
   Q : Q_Table := (others => (others => 0.0));
   
   -- Reward matrix
   R : Reward_Matrix := (
      (0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0)
   );
   
   -- Helper function to get random integer
   function Random_Int(Low, High : Integer) return Integer is
   begin
      return Integer(Random * Float(High - Low + 1)) + Low;
   end Random_Int;
   
   -- Epsilon-greedy policy
   function Choose_Action(State_Index : State) return Action is
      Rand : constant Float := Random;
      Max_Q : Float := Q(State_Index, 0);
      Best_Action : Action := 0;
   begin
      -- Exploration: with probability epsilon, choose random action
      if Rand < Epsilon then
         return Action(Random_Int(0, Num_Actions - 1));
      end if;
      
      -- Exploitation: choose action with highest Q-value
      for A in Action loop
         if Q(State_Index, A) > Max_Q then
            Max_Q := Q(State_Index, A);
            Best_Action := A;
         end if;
      end loop;
      
      return Best_Action;
   end Choose_Action;
   
   -- Q-learning update rule
   procedure Update_Q(State_Index, Action_Index : State; 
                      Next_State : State; Reward : Float) is
      Q_Old : constant Float := Q(State_Index, Action_Index);
      Q_Max : Float := Q(Next_State, 0);
   begin
      -- Find maximum Q-value for next state
      for A in Action loop
         if Q(Next_State, A) > Q_Max then
            Q_Max := Q(Next_State, A);
         end if;
      end loop;
      
      -- Q-learning update equation
      Q(State_Index, Action_Index) := 
         Q_Old + Alpha * (Reward + Gamma * Q_Max - Q_Old);
   end Update_Q;
   
   -- Print Q-table
   procedure Print_Q_Table is
   begin
      Put_Line("Q-Table:");
      for S in State loop
         Put("State " & Integer'Image(Integer(S)) & ": ");
         for A in Action loop
            Put(Float'Image(Q(S, A)));
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Print_Q_Table;
   
   -- Main Q-learning loop
   procedure Learn is
      Current_State : State := 0;
      Next_State : State;
      Action_Index : Action;
      Reward : Float;
      Episodes : constant := 1000;
   begin
      Put_Line("Starting Q-Learning...");
      
      -- Simulate learning process
      for Episode in 1 .. Episodes loop
         Current_State := 0;
         
         -- Run one episode
         for Step in 1 .. 10 loop
            -- Choose action using epsilon-greedy policy
            Action_Index := Choose_Action(Current_State);
            
            -- Simulate environment transition (simple example)
            -- In a real environment, this would be determined by the actual state transition
            case Current_State is
               when 0 =>
                  if Action_Index = 1 then
                     Next_State := 1;
                     Reward := 1.0;
                  else
                     Next_State := 0;
                     Reward := -0.1;
                  end if;
               when 1 =>
                  if Action_Index = 2 then
                     Next_State := 2;
                     Reward := 1.0;
                  else
                     Next_State := 1;
                     Reward := -0.1;
                  end if;
               when 2 =>
                  if Action_Index = 3 then
                     Next_State := 3;
                     Reward := 10.0;
                  else
                     Next_State := 2;
                     Reward := -0.1;
                  end if;
               when 3 =>
                  Next_State := 3;
                  Reward := 0.0;
            end case;
            
            -- Update Q-value
            Update_Q(Current_State, Action_Index, Next_State, Reward);
            
            -- Move to next state
            Current_State := Next_State;
            
            -- Break if reached terminal state
            exit when Current_State = 3;
         end loop;
      end loop;
      
      Put_Line("Learning completed!");
      Print_Q_Table;
   end Learn;

begin
   -- Initialize random seed
   Set_Seed(12345);
   
   -- Run Q-learning
   Learn;
   
end Q_Learning;
```

## Key Components Explained

### 1. **Data Structures**
- `State` and `Action` types define the discrete state and action spaces
- `Q_Table` is a 2D array storing Q-values for each state-action pair
- `Reward_Matrix` stores immediate rewards for state-action transitions

### 2. **Q-Learning Parameters**
- **Alpha (α)**: Learning rate (0.1) - controls how much new information overrides old information
- **Gamma (γ)**: Discount factor (0.9) - determines the importance of future rewards
- **Epsilon (ε)**: Exploration rate (0.1) - probability of choosing random actions

### 3. **Core Functions**
- `Choose_Action`: Implements epsilon-greedy policy for action selection
- `Update_Q`: Implements the Q-learning update rule
- `Learn`: Main learning loop that runs multiple episodes

### 4. **Q-Learning Update Equation**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

This implementation demonstrates the fundamental principles of Q-learning in Ada, including exploration vs exploitation, value iteration, and policy improvement.

