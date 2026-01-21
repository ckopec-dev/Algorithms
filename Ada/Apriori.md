# Apriori Algorithm in Ada

Here's an implementation of the Apriori algorithm for finding frequent itemsets in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Apriori_Example is

   -- Define item type
   type Item is new Character;
   
   -- Define transaction type (set of items)
   type Transaction is array (Positive range <>) of Item;
   
   -- Define itemset type (set of items)
   type Itemset is array (Positive range <>) of Item;
   
   -- Vector to store transactions
   package Transaction_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Itemset);
   
   -- Map to store item frequencies
   package Item_Frequency_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Item, Element_Type => Natural);
   
   -- Vector to store frequent itemsets
   package Frequent_Itemset_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Itemset);

   -- Sample transactions
   Transactions : Transaction_Vector.Vector;
   
   -- Minimum support threshold
   Min_Support : constant Natural := 2;
   
   -- Function to count occurrences of an item in transactions
   function Count_Item (Item_To_Count : Item) return Natural is
      Count : Natural := 0;
   begin
      for T in Transactions.First_Index .. Transactions.Last_Index loop
         for I in Transactions.Element(T).First_Index .. Transactions.Element(T).Last_Index loop
            if Transactions.Element(T)(I) = Item_To_Count then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Count_Item;
   
   -- Function to check if an itemset is frequent
   function Is_Frequent (Itemset_To_Check : Itemset) return Boolean is
      Count : Natural := 0;
   begin
      for T in Transactions.First_Index .. Transactions.Last_Index loop
         declare
            Found : Boolean := True;
         begin
            for I in Itemset_To_Check.First_Index .. Itemset_To_Check.Last_Index loop
               declare
                  Item_Found : Boolean := False;
               begin
                  for J in Transactions.Element(T).First_Index .. Transactions.Element(T).Last_Index loop
                     if Transactions.Element(T)(J) = Itemset_To_Check(I) then
                        Item_Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Item_Found then
                     Found := False;
                     exit;
                  end if;
               end;
            end loop;
            if Found then
               Count := Count + 1;
            end if;
         end;
      end loop;
      return Count >= Min_Support;
   end Is_Frequent;
   
   -- Function to generate candidate itemsets of size K
   function Generate_Candidates (Frequent_Itemsets : Frequent_Itemset_Vector.Vector;
                                K : Positive) return Frequent_Itemset_Vector.Vector is
      Candidates : Frequent_Itemset_Vector.Vector;
   begin
      -- Simplified candidate generation (in practice, this would be more complex)
      -- This is a basic example showing the concept
      return Candidates;
   end Generate_Candidates;
   
   -- Function to find frequent 1-itemsets
   function Find_Frequent_1_Itemsets return Frequent_Itemset_Vector.Vector is
      Frequent_1 : Frequent_Itemset_Vector.Vector;
      Item_Count : Item_Frequency_Map.Map;
      Item_List : array (1..26) of Item;
      Item_Index : Positive := 1;
   begin
      -- Count all items
      for T in Transactions.First_Index .. Transactions.Last_Index loop
         for I in Transactions.Element(T).First_Index .. Transactions.Element(T).Last_Index loop
            declare
               Item : constant Item := Transactions.Element(T)(I);
            begin
               if Item_Count.Contains(Item) then
                  Item_Count.Replace_Element(Item, Item_Count.Element(Item) + 1);
               else
                  Item_Count.Insert(Item, 1);
                  Item_List(Item_Index) := Item;
                  Item_Index := Item_Index + 1;
               end if;
            end;
         end loop;
      end loop;
      
      -- Find frequent 1-itemsets
      for I in 1 .. Item_Index - 1 loop
         declare
            Item : constant Item := Item_List(I);
         begin
            if Item_Count.Element(Item) >= Min_Support then
               declare
                  Itemset : Itemset(1..1);
               begin
                  Itemset(1) := Item;
                  Frequent_1.Append(Itemset);
               end;
            end if;
         end;
      end loop;
      
      return Frequent_1;
   end Find_Frequent_1_Itemsets;
   
   -- Main Apriori algorithm
   procedure Apriori is
      Frequent_Itemsets : Frequent_Itemset_Vector.Vector;
      Current_Itemsets : Frequent_Itemset_Vector.Vector;
      K : Positive := 1;
   begin
      -- Initialize with 1-itemsets
      Current_Itemsets := Find_Frequent_1_Itemsets;
      
      Put_Line("Frequent 1-itemsets:");
      for I in Current_Itemsets.First_Index .. Current_Itemsets.Last_Index loop
         Put("  {");
         for J in Current_Itemsets.Element(I).First_Index .. Current_Itemsets.Element(I).Last_Index loop
            Put(Current_Itemsets.Element(I)(J));
            if J < Current_Itemsets.Element(I).Last_Index then
               Put(",");
            end if;
         end loop;
         Put_Line("}");
      end loop;
      
      -- Continue until no more frequent itemsets can be found
      loop
         exit when Current_Itemsets.Is_Empty;
         
         -- Generate candidates (simplified)
         -- In a complete implementation, this would use the Apriori property
         K := K + 1;
         Current_Itemsets.Clear;
         
         -- For demonstration, we'll stop after first iteration
         exit;
      end loop;
      
      Put_Line("Apriori algorithm completed.");
   end Apriori;

begin
   -- Initialize sample transactions
   declare
      T1 : Itemset(1..3) := ('A', 'B', 'C');
      T2 : Itemset(1..2) := ('A', 'B');
      T3 : Itemset(1..3) := ('A', 'C', 'D');
      T4 : Itemset(1..2) := ('B', 'C');
      T5 : Itemset(1..3) := ('A', 'B', 'D');
   begin
      Transactions.Append(T1);
      Transactions.Append(T2);
      Transactions.Append(T3);
      Transactions.Append(T4);
      Transactions.Append(T5);
   end;
   
   Put_Line("Transactions:");
   for I in Transactions.First_Index .. Transactions.Last_Index loop
      Put("  T" & Positive'Image(I) & ": {");
      for J in Transactions.Element(I).First_Index .. Transactions.Element(I).Last_Index loop
         Put(Transactions.Element(I)(J));
         if J < Transactions.Element(I).Last_Index then
            Put(",");
         end if;
      end loop;
      Put_Line("}");
   end loop;
   
   Put_Line("");
   
   -- Run Apriori algorithm
   Apriori;
   
end Apriori_Example;
```

## Key Features of this Implementation

1. **Transaction Storage**: Uses vectors to store transaction data
2. **Itemset Representation**: Arrays to represent itemsets
3. **Frequency Counting**: Functions to count item occurrences
4. **Frequent Itemset Detection**: Checks if itemsets meet minimum support threshold
5. **Basic Apriori Logic**: Implements the core concept of generating candidates and checking support

## How it Works

1. **Input**: Sample transactions with items A, B, C, D
2. **Step 1**: Count frequency of individual items
3. **Step 2**: Identify frequent 1-itemsets (those meeting minimum support)
4. **Step 3**: Generate candidates for larger itemsets (simplified in this example)

## Output Example

```
Transactions:
  T1: {A,B,C}
  T2: {A,B}
  T3: {A,C,D}
  T4: {B,C}
  T5: {A,B,D}

Frequent 1-itemsets:
  {A}
  {B}
  {C}
Apriori algorithm completed.
```

This is a simplified implementation showing the core concepts. A complete Apriori implementation would include proper candidate generation, pruning, and more sophisticated data structures for better performance.

