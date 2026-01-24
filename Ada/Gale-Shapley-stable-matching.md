# Gale-Shapley Stable Matching Algorithm in Ada

Here's a complete implementation of the Gale-Shapley algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Gale_Shapley is
   
   -- Constants
   MAX_PEOPLE := 10;
   MAX_RANKS := 10;
   
   -- Types
   type Person is range 1..MAX_PEOPLE;
   type Rank is range 1..MAX_RANKS;
   
   type Preference_List is array (Person) of Person;
   type Preference_Matrix is array (Person, Person) of Rank;
   
   type Man is record
      Name : String(1..10);
      Index : Person;
      Preference : Preference_List;
      Current_Match : Person := 0;
      Next_Proposal : Person := 1;
   end record;
   
   type Woman is record
      Name : String(1..10);
      Index : Person;
      Preference : Preference_List;
      Current_Match : Person := 0;
   end record;
   
   type Men_Array is array (Person) of Man;
   type Women_Array is array (Person) of Woman;
   
   -- Sample data
   Men : Men_Array(1..4) := (
      (Name => "Adam", Index => 1, Preference => (1, 2, 3, 4), Current_Match => 0, Next_Proposal => 1),
      (Name => "Bob",  Index => 2, Preference => (3, 1, 2, 4), Current_Match => 0, Next_Proposal => 1),
      (Name => "Charlie", Index => 3, Preference => (2, 3, 1, 4), Current_Match => 0, Next_Proposal => 1),
      (Name => "David", Index => 4, Preference => (4, 3, 2, 1), Current_Match => 0, Next_Proposal => 1)
   );
   
   Women : Women_Array(1..4) := (
      (Name => "Alice", Index => 1, Preference => (1, 2, 3, 4), Current_Match => 0),
      (Name => "Betty", Index => 2, Preference => (2, 1, 4, 3), Current_Match => 0),
      (Name => "Cathy", Index => 3, Preference => (3, 4, 1, 2), Current_Match => 0),
      (Name => "Diana", Index => 4, Preference => (4, 3, 2, 1), Current_Match => 0)
   );
   
   -- Function to find the rank of a person in another's preference list
   function Get_Rank(Who : Person; Whom : Person; List : Preference_List) return Rank is
   begin
      for i in List'Range loop
         if List(i) = Whom then
            return i;
         end if;
      end loop;
      return 0;
   end Get_Rank;
   
   -- Function to check if a man prefers woman over current match
   function Prefer_Over_Current(Man_Index : Person; Woman_Index : Person) return Boolean is
      Man_Pref : constant Person := Men(Man_Index).Current_Match;
   begin
      if Man_Pref = 0 then
         return True;
      else
         return Get_Rank(Man_Index, Woman_Index, Men(Man_Index).Preference) < 
                Get_Rank(Man_Index, Man_Pref, Men(Man_Index).Preference);
      end if;
   end Prefer_Over_Current;
   
   -- Function to check if woman prefers man over current match
   function Woman_Prefer_Over_Current(Woman_Index : Person; Man_Index : Person) return Boolean is
      Woman_Pref : constant Person := Women(Woman_Index).Current_Match;
   begin
      if Woman_Pref = 0 then
         return True;
      else
         return Get_Rank(Woman_Index, Man_Index, Women(Woman_Index).Preference) < 
                Get_Rank(Woman_Index, Woman_Pref, Women(Woman_Index).Preference);
      end if;
   end Woman_Prefer_Over_Current;
   
   -- Main Gale-Shapley algorithm
   procedure Run_Gale_Shapley is
      Unmatched_Men : array (Person) of Boolean;
      Count : Person := 0;
   begin
      -- Initialize all men as unmatched
      for i in Person'Range loop
         Unmatched_Men(i) := True;
         Count := Count + 1;
      end loop;
      
      -- Continue until all men are matched
      while Count > 0 loop
         declare
            Current_Man : Person;
            Current_Woman : Person;
         begin
            -- Find an unmatched man
            Current_Man := 0;
            for i in Person'Range loop
               if Unmatched_Men(i) then
                  Current_Man := i;
                  exit;
               end if;
            end loop;
            
            -- Get next woman to propose to
            Current_Woman := Men(Current_Man).Preference(Men(Current_Man).Next_Proposal);
            
            -- Increment next proposal index
            Men(Current_Man).Next_Proposal := Men(Current_Man).Next_Proposal + 1;
            
            -- If woman is unmatched, match them
            if Women(Current_Woman).Current_Match = 0 then
               -- Update matches
               Men(Current_Man).Current_Match := Current_Woman;
               Women(Current_Woman).Current_Match := Current_Man;
               Unmatched_Men(Current_Man) := False;
               Count := Count - 1;
            else
               -- Woman is matched, check if she prefers this man
               if Woman_Prefer_Over_Current(Current_Woman, Current_Man) then
                  -- She accepts the proposal
                  declare
                     Old_Man : constant Person := Women(Current_Woman).Current_Match;
                  begin
                     -- Break old match
                     Men(Old_Man).Current_Match := 0;
                     Unmatched_Men(Old_Man) := True;
                     Count := Count + 1;
                     
                     -- Create new match
                     Men(Current_Man).Current_Match := Current_Woman;
                     Women(Current_Woman).Current_Match := Current_Man;
                     Unmatched_Men(Current_Man) := False;
                     Count := Count - 1;
                  end;
               end if;
            end if;
         end;
      end loop;
   end Run_Gale_Shapley;
   
   -- Print final matching
   procedure Print_Matching is
   begin
      Put_Line("Final Stable Matching:");
      Put_Line("----------------------");
      for i in Person'Range loop
         if Men(i).Current_Match /= 0 then
            Put_Line(Men(i).Name & " is matched with " & Women(Men(i).Current_Match).Name);
         end if;
      end loop;
   end Print_Matching;
   
begin
   Put_Line("Gale-Shapley Stable Matching Algorithm");
   Put_Line("======================================");
   
   -- Display initial preferences
   Put_Line("Men's Preferences:");
   for i in Person'Range loop
      Put("  " & Men(i).Name & ": ");
      for j in Men(i).Preference'Range loop
         Put(Women(Men(i).Preference(j)).Name & " ");
      end loop;
      New_Line;
   end loop;
   
   Put_Line("Women's Preferences:");
   for i in Person'Range loop
      Put("  " & Women(i).Name & ": ");
      for j in Women(i).Preference'Range loop
         Put(Men(Women(i).Preference(j)).Name & " ");
      end loop;
      New_Line;
   end loop;
   
   -- Run the algorithm
   Run_Gale_Shapley;
   
   -- Print results
   Print_Matching;
   
end Gale_Shapley;
```

## How it works:

1. **Data Structures**: 
   - `Man` and `Woman` records store names, preferences, current matches, and proposal tracking
   - Preference lists are stored as arrays of person indices

2. **Algorithm Steps**:
   - Initialize all men as unmatched
   - While there are unmatched men:
     - Pick an unmatched man
     - He proposes to the next woman on his preference list
     - If woman is unmatched, they get matched
     - If woman is matched, she accepts if she prefers this man over her current match

3. **Key Functions**:
   - `Get_Rank`: Finds the position of a person in another's preference list
   - `Prefer_Over_Current`: Checks if a man prefers a woman over his current match
   - `Woman_Prefer_Over_Current`: Checks if a woman prefers a man over her current match

4. **Output**: The program displays the initial preferences and then shows the final stable matching.

This implementation guarantees a stable matching where no man and woman who are not matched to each other would both prefer each other over their current partners.

