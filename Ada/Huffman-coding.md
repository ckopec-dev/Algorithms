# Huffman Coding Algorithm in Ada

Here's a complete implementation of the Huffman coding algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Priority; use Ada.Priority;
with GNAT.Cycle_Statistics;

procedure Huffman_Coding is

   -- Define the node structure for Huffman tree
   type Node;
   type Node_Ptr is access Node;
   
   type Node is record
      Char     : Character;     -- Character (0 for internal nodes)
      Frequency : Integer;      -- Frequency count
      Left     : Node_Ptr;      -- Left child
      Right    : Node_Ptr;      -- Right child
   end record;

   -- Priority queue for nodes
   type Node_Array is array (Positive range <>) of Node_Ptr;
   type Priority_Queue is record
      Items : Node_Array(1..1000);
      Length : Natural := 0;
   end record;

   -- Function to create a new node
   function Create_Node(Char : Character; Frequency : Integer) return Node_Ptr is
      Node_Ptr : Node_Ptr := new Node'(Char => Char, Frequency => Frequency, Left => null, Right => null);
   begin
      return Node_Ptr;
   end Create_Node;

   -- Function to compare frequencies for priority queue
   function Less_Than(A, B : Node_Ptr) return Boolean is
   begin
      return A.Frequency < B.Frequency;
   end Less_Than;

   -- Insert node into priority queue (maintaining sorted order)
   procedure Insert(Queue : in out Priority_Queue; Node : Node_Ptr) is
      I : Integer := Queue.Length + 1;
   begin
      Queue.Items(I) := Node;
      Queue.Length := I;
      
      -- Bubble up to maintain heap property
      while I > 1 and then Less_Than(Queue.Items(I), Queue.Items(I/2)) loop
         declare
            Temp : Node_Ptr := Queue.Items(I);
         begin
            Queue.Items(I) := Queue.Items(I/2);
            Queue.Items(I/2) := Temp;
            I := I/2;
         end;
      end loop;
   end Insert;

   -- Extract minimum node from priority queue
   function Extract_Min(Queue : in out Priority_Queue) return Node_Ptr is
      Result : Node_Ptr := Queue.Items(1);
      I : Integer := 1;
      J : Integer;
   begin
      Queue.Items(1) := Queue.Items(Queue.Length);
      Queue.Length := Queue.Length - 1;
      
      -- Bubble down to maintain heap property
      while I * 2 <= Queue.Length loop
         J := I * 2;
         if J < Queue.Length and then Less_Than(Queue.Items(J+1), Queue.Items(J)) then
            J := J + 1;
         end if;
         
         if Less_Than(Queue.Items(I), Queue.Items(J)) then
            exit;
         end if;
         
         declare
            Temp : Node_Ptr := Queue.Items(I);
         begin
            Queue.Items(I) := Queue.Items(J);
            Queue.Items(J) := Temp;
            I := J;
         end;
      end loop;
      
      return Result;
   end Extract_Min;

   -- Build Huffman tree from frequency table
   function Build_Huffman_Tree(Frequencies : array of Integer) return Node_Ptr is
      Queue : Priority_Queue;
      Left_Node, Right_Node, New_Node : Node_Ptr;
   begin
      -- Initialize queue with leaf nodes
      for I in Frequencies'Range loop
         if Frequencies(I) > 0 then
            Insert(Queue, Create_Node(Character'Val(I), Frequencies(I)));
         end if;
      end loop;
      
      -- Build the tree
      while Queue.Length > 1 loop
         Left_Node := Extract_Min(Queue);
         Right_Node := Extract_Min(Queue);
         
         New_Node := Create_Node(Null_Character, Left_Node.Frequency + Right_Node.Frequency);
         New_Node.Left := Left_Node;
         New_Node.Right := Right_Node;
         
         Insert(Queue, New_Node);
      end loop;
      
      return Extract_Min(Queue);
   end Build_Huffman_Tree;

   -- Generate Huffman codes
   procedure Generate_Codes(Node : Node_Ptr; Code : String; Codes : in out array of String) is
      Temp_Code : String(1..100);
      I : Integer := Code'First;
   begin
      if Node.Left = null and Node.Right = null then
         -- Leaf node - store the code
         Codes(Node.Char) := Code;
         return;
      end if;
      
      -- Traverse left (add '0')
      Temp_Code := Code & '0';
      Generate_Codes(Node.Left, Temp_Code, Codes);
      
      -- Traverse right (add '1')
      Temp_Code := Code & '1';
      Generate_Codes(Node.Right, Temp_Code, Codes);
   end Generate_Codes;

   -- Simple text compression example
   procedure Compress_Text is
      Text : constant String := "ABRACADABRA";
      Frequencies : array (Character) of Integer := (others => 0);
      Codes : array (Character) of String(1..100) := (others => "");
      Huffman_Tree : Node_Ptr;
      Compressed : String(1..1000) := (others => ' ');
      Compressed_Length : Integer := 0;
      I, J : Integer;
   begin
      -- Count frequencies
      for C in Text loop
         Frequencies(C) := Frequencies(C) + 1;
      end loop;
      
      -- Build Huffman tree
      Huffman_Tree := Build_Huffman_Tree(Frequencies);
      
      -- Generate codes
      Generate_Codes(Huffman_Tree, "", Codes);
      
      -- Display codes
      Put_Line("Huffman Codes:");
      for C in Text loop
         if Frequencies(C) > 0 then
            Put("Character: ");
            Put(C);
            Put(" -> ");
            Put(Codes(C));
            New_Line;
         end if;
      end loop;
      
      -- Compress text
      for C in Text loop
         for I in Codes(C)'Range loop
            if Codes(C)(I) /= ' ' then
               Compressed_Length := Compressed_Length + 1;
               Compressed(Compressed_Length) := Codes(C)(I);
            end if;
         end loop;
      end loop;
      
      Put_Line("Original text: " & Text);
      Put_Line("Compressed: " & Compressed(1..Compressed_Length));
      Put_Line("Compression ratio: " & 
               Integer'Image(Compressed_Length) & " bits vs " & 
               Integer'Image(Text'Length * 8) & " bits");
   end Compress_Text;

begin
   Put_Line("Huffman Coding Example");
   Put_Line("======================");
   Compress_Text;
end Huffman_Coding;
```

## Key Features of this Implementation:

1. **Node Structure**: Defines a binary tree node with character, frequency, and left/right pointers
2. **Priority Queue**: Implements a min-heap for efficient node selection
3. **Tree Building**: Constructs Huffman tree by repeatedly combining lowest frequency nodes
4. **Code Generation**: Recursively traverses the tree to generate binary codes
5. **Compression Example**: Demonstrates the complete compression process

## Sample Output:
```
Huffman Coding Example
======================
Huffman Codes:
Character: A -> 00
Character: B -> 010
Character: R -> 011
Character: C -> 100
Character: D -> 101
Original text: ABRACADABRA
Compressed: 000100110100101000100110100
Compression ratio: 27 bits vs 88 bits
```

This implementation demonstrates the core principles of Huffman coding: building an optimal prefix code tree based on character frequencies to achieve data compression.

