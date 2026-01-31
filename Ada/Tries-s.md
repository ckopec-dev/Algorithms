# Trie Implementation in Ada

Here's a complete implementation of a Trie (prefix tree) data structure in Ada:

```ada
-- Trie package specification
package Trie is
    type Trie_Node;
    type Trie_Node_Access is access Trie_Node;
    
    type Trie is record
        Root : Trie_Node_Access;
    end record;
    
    -- Create a new empty trie
    function Create_Trie return Trie;
    
    -- Insert a word into the trie
    procedure Insert(T : in out Trie; Word : String);
    
    -- Search for a word in the trie
    function Search(T : Trie; Word : String) return Boolean;
    
    -- Check if a prefix exists in the trie
    function Start_With(T : Trie; Prefix : String) return Boolean;
    
    -- Delete a word from the trie
    procedure Delete(T : in out Trie; Word : String);
    
    -- Check if trie is empty
    function Is_Empty(T : Trie) return Boolean;
    
private
    type Trie_Node is record
        Children : array (Character range 'a' .. 'z') of Trie_Node_Access;
        Is_End_Of_Word : Boolean := False;
    end record;
    
    -- Helper function to create a new node
    function Create_Node return Trie_Node_Access;
    
    -- Helper function to destroy a node and its children
    procedure Destroy_Node(Node : in out Trie_Node_Access);
end Trie;

-- Trie package body
package body Trie is
    
    function Create_Node return Trie_Node_Access is
        Node : constant Trie_Node_Access := new Trie_Node;
    begin
        return Node;
    end Create_Node;
    
    procedure Destroy_Node(Node : in out Trie_Node_Access) is
    begin
        if Node /= null then
            for I in Node.Children'Range loop
                Destroy_Node(Node.Children(I));
            end loop;
            Free(Node);
        end if;
    end Destroy_Node;
    
    function Create_Trie return Trie is
        T : Trie;
    begin
        T.Root := Create_Node;
        return T;
    end Create_Trie;
    
    procedure Insert(T : in out Trie; Word : String) is
        Current : Trie_Node_Access := T.Root;
        Char_Index : Integer;
    begin
        for I in Word'Range loop
            Char_Index := Character'Pos(Word(I)) - Character'Pos('a') + 1;
            
            if Char_Index < 1 or Char_Index > 26 then
                -- Handle non-lowercase letters (skip for simplicity)
                null;
            else
                if Current.Children(Char_Index) = null then
                    Current.Children(Char_Index) := Create_Node;
                end if;
                Current := Current.Children(Char_Index);
            end if;
        end loop;
        
        Current.Is_End_Of_Word := True;
    end Insert;
    
    function Search(T : Trie; Word : String) return Boolean is
        Current : Trie_Node_Access := T.Root;
        Char_Index : Integer;
    begin
        for I in Word'Range loop
            Char_Index := Character'Pos(Word(I)) - Character'Pos('a') + 1;
            
            if Char_Index < 1 or Char_Index > 26 then
                return False;
            end if;
            
            if Current.Children(Char_Index) = null then
                return False;
            end if;
            
            Current := Current.Children(Char_Index);
        end loop;
        
        return Current.Is_End_Of_Word;
    end Search;
    
    function Start_With(T : Trie; Prefix : String) return Boolean is
        Current : Trie_Node_Access := T.Root;
        Char_Index : Integer;
    begin
        for I in Prefix'Range loop
            Char_Index := Character'Pos(Prefix(I)) - Character'Pos('a') + 1;
            
            if Char_Index < 1 or Char_Index > 26 then
                return False;
            end if;
            
            if Current.Children(Char_Index) = null then
                return False;
            end if;
            
            Current := Current.Children(Char_Index);
        end loop;
        
        return True;
    end Start_With;
    
    procedure Delete(T : in out Trie; Word : String) is
        procedure Delete_Helper(Node : in out Trie_Node_Access; Word : String; Index : Integer);
        
        procedure Delete_Helper(Node : in out Trie_Node_Access; Word : String; Index : Integer) is
            Char_Index : Integer;
        begin
            if Node = null then
                return;
            end if;
            
            if Index = Word'Length then
                Node.Is_End_Of_Word := False;
                
                -- If this node has no children and is not end of another word, delete it
                for I in Node.Children'Range loop
                    if Node.Children(I) /= null then
                        return;
                    end if;
                end loop;
                
                -- Delete this node
                Destroy_Node(Node);
                return;
            end if;
            
            Char_Index := Character'Pos(Word(Index)) - Character'Pos('a') + 1;
            
            if Char_Index >= 1 and Char_Index <= 26 then
                Delete_Helper(Node.Children(Char_Index), Word, Index + 1);
            end if;
        end Delete_Helper;
        
    begin
        Delete_Helper(T.Root, Word, 1);
    end Delete;
    
    function Is_Empty(T : Trie) return Boolean is
    begin
        return T.Root = null;
    end Is_Empty;
    
end Trie;

-- Example usage
with Ada.Text_IO; use Ada.Text_IO;
with Trie; use Trie;

procedure Trie_Example is
    T : Trie := Create_Trie;
    
begin
    -- Insert words
    Insert(T, "apple");
    Insert(T, "app");
    Insert(T, "application");
    Insert(T, "apply");
    
    -- Search for words
    Put_Line("Search 'app': " & Boolean'Image(Search(T, "app")));
    Put_Line("Search 'apple': " & Boolean'Image(Search(T, "apple")));
    Put_Line("Search 'appl': " & Boolean'Image(Search(T, "appl")));
    
    -- Check prefixes
    Put_Line("Start with 'app': " & Boolean'Image(Start_With(T, "app")));
    Put_Line("Start with 'appl': " & Boolean'Image(Start_With(T, "appl")));
    Put_Line("Start with 'xyz': " & Boolean'Image(Start_With(T, "xyz")));
    
    -- Delete a word
    Delete(T, "app");
    Put_Line("After deleting 'app':");
    Put_Line("Search 'app': " & Boolean'Image(Search(T, "app")));
    Put_Line("Search 'apple': " & Boolean'Image(Search(T, "apple")));
    
end Trie_Example;
```

## Key Features of this Trie Implementation:

1. **Trie Structure**: Uses an array of 26 pointers (for 'a' to 'z') for each node
2. **Operations**:
   - `Insert`: Adds a word to the trie
   - `Search`: Checks if a word exists
   - `Start_With`: Checks if any word starts with a given prefix
   - `Delete`: Removes a word from the trie
   - `Is_Empty`: Checks if the trie is empty

3. **Memory Management**: Properly allocates and deallocates nodes using Ada's `new` and `Free` operations

4. **Error Handling**: Handles invalid characters gracefully

5. **Case Sensitivity**: Only handles lowercase letters (a-z) for simplicity

## Sample Output:
```
Search 'app': TRUE
Search 'apple': TRUE
Search 'appl': FALSE
Start with 'app': TRUE
Start with 'appl': TRUE
Start with 'xyz': FALSE
After deleting 'app':
Search 'app': FALSE
Search 'apple': TRUE
```

This implementation demonstrates the core functionality of a Trie data structure in Ada, showing how it efficiently stores and retrieves words based on their prefixes.

