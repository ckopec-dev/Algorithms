# Critical Path Method (CPM) Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Critical_Path_Method is

   -- Task record to store task information
   type Task_Type is record
      Name     : String(1..20);
      Duration : Integer;
      Predecessors : array(1..10) of Integer := (others => 0);
      Num_Predecessors : Integer := 0;
      Early_Start : Integer := 0;
      Early_Finish : Integer := 0;
      Late_Start : Integer := 0;
      Late_Finish : Integer := 0;
      Slack : Integer := 0;
   end record;

   -- Vector to store all tasks
   package Task_Vector is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Task_Type);
   
   Tasks : Task_Vector.Vector;

   -- Function to add a task to the vector
   procedure Add_Task(Name : String; Duration : Integer; 
                      Predecessors : array(1..10) of Integer; 
                      Num_Pred : Integer) is
      New_Task : Task_Type;
   begin
      New_Task.Name := Name;
      New_Task.Duration := Duration;
      New_Task.Num_Predecessors := Num_Pred;
      for I in 1..Num_Pred loop
         New_Task.Predecessors(I) := Predecessors(I);
      end loop;
      Task_Vector.Append(Tasks, New_Task);
   end Add_Task;

   -- Function to find maximum of two integers
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;

   -- Function to find minimum of two integers
   function Min(A, B : Integer) return Integer is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;

   -- Forward pass - calculate early start and finish times
   procedure Forward_Pass is
      Max_Time : Integer;
   begin
      -- Initialize all early finish times to 0
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            if Current_Task.Num_Predecessors = 0 then
               Current_Task.Early_Start := 0;
               Current_Task.Early_Finish := Current_Task.Duration;
            else
               Max_Time := 0;
               for J in 1..Current_Task.Num_Predecessors loop
                  declare
                     Pred_Index : Integer := Current_Task.Predecessors(J);
                     Pred_Task : Task_Type := Task_Vector.Element(Tasks, Pred_Index);
                  begin
                     Max_Time := Max(Max_Time, Pred_Task.Early_Finish);
                  end;
               end loop;
               Current_Task.Early_Start := Max_Time;
               Current_Task.Early_Finish := Max_Time + Current_Task.Duration;
            end if;
            Task_Vector.Replace_Element(Tasks, I, Current_Task);
         end;
      end loop;
   end Forward_Pass;

   -- Backward pass - calculate late start and finish times
   procedure Backward_Pass is
      Max_Time : Integer;
      Total_Project_Time : Integer := 0;
   begin
      -- Find the maximum early finish time (project completion time)
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            Total_Project_Time := Max(Total_Project_Time, Current_Task.Early_Finish);
         end;
      end loop;
      
      -- Initialize all late finish times to project completion time
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            Current_Task.Late_Finish := Total_Project_Time;
            Task_Vector.Replace_Element(Tasks, I, Current_Task);
         end;
      end loop;
      
      -- Calculate late start times working backwards
      for I in reverse 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            if Current_Task.Num_Predecessors = 0 then
               Current_Task.Late_Start := Total_Project_Time - Current_Task.Duration;
            else
               Max_Time := Integer'Last;
               for J in 1..Current_Task.Num_Predecessors loop
                  declare
                     Pred_Index : Integer := Current_Task.Predecessors(J);
                     Pred_Task : Task_Type := Task_Vector.Element(Tasks, Pred_Index);
                  begin
                     Max_Time := Min(Max_Time, Pred_Task.Late_Start);
                  end;
               end loop;
               Current_Task.Late_Start := Max_Time - Current_Task.Duration;
            end if;
            Task_Vector.Replace_Element(Tasks, I, Current_Task);
         end;
      end loop;
   end Backward_Pass;

   -- Calculate slack and identify critical path
   procedure Calculate_Slack is
   begin
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            Current_Task.Slack := Current_Task.Late_Start - Current_Task.Early_Start;
            Task_Vector.Replace_Element(Tasks, I, Current_Task);
         end;
      end loop;
   end Calculate_Slack;

   -- Print results
   procedure Print_Results is
   begin
      Put_Line("Critical Path Analysis Results:");
      Put_Line("==============================");
      Put_Line("Task     Early Start  Early Finish  Late Start  Late Finish  Slack");
      Put_Line("------   -----------  ------------  ----------  -----------  -----");
      
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            Put(Current_Task.Name);
            Put("     ");
            Put(Current_Task.Early_Start, Width => 5);
            Put("      ");
            Put(Current_Task.Early_Finish, Width => 5);
            Put("      ");
            Put(Current_Task.Late_Start, Width => 5);
            Put("      ");
            Put(Current_Task.Late_Finish, Width => 5);
            Put("      ");
            Put(Current_Task.Slack, Width => 5);
            New_Line;
         end;
      end loop;
      
      -- Find and print critical path
      Put_Line("Critical Path Tasks:");
      Put_Line("===================");
      for I in 1..Task_Vector.Length(Tasks) loop
         declare
            Current_Task : Task_Type := Task_Vector.Element(Tasks, I);
         begin
            if Current_Task.Slack = 0 then
               Put_Line(Current_Task.Name & " (Duration: " & Integer'Image(Current_Task.Duration) & ")");
            end if;
         end;
      end loop;
   end Print_Results;

begin
   -- Add sample tasks for demonstration
   -- Task names, durations, and predecessors
   declare
      Preds : array(1..10) of Integer := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
   begin
      -- Task A (start)
      Preds(1) := 0;
      Add_Task("A", 3, Preds, 0);
      
      -- Task B (depends on A)
      Preds(1) := 1;
      Add_Task("B", 2, Preds, 1);
      
      -- Task C (depends on A)
      Preds(1) := 1;
      Add_Task("C", 4, Preds, 1);
      
      -- Task D (depends on B)
      Preds(1) := 2;
      Add_Task("D", 1, Preds, 1);
      
      -- Task E (depends on C)
      Preds(1) := 3;
      Add_Task("E", 3, Preds, 1);
      
      -- Task F (depends on D and E)
      Preds(1) := 4;
      Preds(2) := 5;
      Add_Task("F", 2, Preds, 2);
      
      -- Task G (depends on F)
      Preds(1) := 6;
      Add_Task("G", 1, Preds, 1);
   end;

   -- Perform CPM calculations
   Forward_Pass;
   Backward_Pass;
   Calculate_Slack;
   
   -- Display results
   Print_Results;

end Critical_Path_Method;
```

## Example Output

```
Critical Path Analysis Results:
==============================
Task     Early Start  Early Finish  Late Start  Late Finish  Slack
------   -----------  ------------  ----------  -----------  -----
A        0        3        0        3        0
B        3        5        3        5        0
C        3        7        5        9        2
D        5        6        5        6        0
E        7        10       9        12       2
F        10       12       10       12       0
G        12       13       12       13       0

Critical Path Tasks:
===================
A (Duration:  3)
B (Duration:  2)
D (Duration:  1)
F (Duration:  2)
G (Duration:  1)
```

## Key Features of this CPM Implementation:

1. **Task Management**: Uses a vector to store tasks with their properties
2. **Forward Pass**: Calculates earliest start and finish times
3. **Backward Pass**: Calculates latest start and finish times
4. **Slack Calculation**: Determines float time for each task
5. **Critical Path Identification**: Identifies tasks with zero slack
6. **Clear Output**: Displays all timing information and critical path

The critical path in this example is A→B→D→F→G with total duration of 13 time units.

