# K-D Tree Search Algorithm in Ada

Here's an implementation of a K-D tree search algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure KDTree_Search is

   -- Define a point in 2D space
   type Point is record
      X, Y : Float;
   end record;

   -- Define a K-D tree node
   type KDNode is record
      Point_Data : Point;
      Left       : access KDNode := null;
      Right      : access KDNode := null;
      Dimension  : Natural;  -- 0 for X, 1 for Y
   end record;

   type KDNode_Access is access KDNode;

   -- K-D Tree root
   type KDTree is record
      Root : KDNode_Access := null;
   end record;

   -- Insert a point into the K-D tree
   procedure Insert(Tree : in out KDTree; New_Point : Point) is
      procedure Insert_Helper(Node : in out KDNode_Access; 
                             Point : Point; 
                             Depth : Natural) is
         Current_Dim : constant Natural := Depth mod 2;
      begin
         if Node = null then
            Node := new KDNode'(Point, null, null, Current_Dim);
         else
            if Point.X < Node.all.Point_Data.X or 
               (Point.X = Node.all.Point_Data.X and Current_Dim = 0) then
               Insert_Helper(Node.all.Left, Point, Depth + 1);
            else
               Insert_Helper(Node.all.Right, Point, Depth + 1);
            end if;
         end if;
      end Insert_Helper;
   begin
      Insert_Helper(Tree.Root, New_Point, 0);
   end Insert;

   -- Search for a point in the K-D tree
   function Search(Tree : KDTree; Target : Point) return Boolean is
      function Search_Helper(Node : KDNode_Access; 
                           Target : Point; 
                           Depth : Natural) return Boolean is
         Current_Dim : constant Natural := Depth mod 2;
      begin
         if Node = null then
            return False;
         end if;

         if Target.X = Node.all.Point_Data.X and 
            Target.Y = Node.all.Point_Data.Y then
            return True;
         end if;

         if Target.X < Node.all.Point_Data.X or 
            (Target.X = Node.all.Point_Data.X and Current_Dim = 0) then
            return Search_Helper(Node.all.Left, Target, Depth + 1);
         else
            return Search_Helper(Node.all.Right, Target, Depth + 1);
         end if;
      end Search_Helper;
   begin
      return Search_Helper(Tree.Root, Target, 0);
   end Search;

   -- Find the nearest neighbor to a target point
   function Find_Nearest(Tree : KDTree; Target : Point) return Point is
      Nearest_Point : Point := (X => 0.0, Y => 0.0);
      Min_Distance  : Float := Float'Last;
      
      procedure Find_Nearest_Helper(Node : KDNode_Access; 
                                   Target : Point; 
                                   Depth : Natural) is
         Current_Dim : constant Natural := Depth mod 2;
         Distance    : Float;
      begin
         if Node = null then
            return;
         end if;

         -- Calculate distance to current node
         Distance := (Target.X - Node.all.Point_Data.X) ** 2 + 
                     (Target.Y - Node.all.Point_Data.Y) ** 2;

         if Distance < Min_Distance then
            Min_Distance := Distance;
            Nearest_Point := Node.all.Point_Data;
         end if;

         -- Decide which subtree to search first
         if Target.X < Node.all.Point_Data.X or 
            (Target.X = Node.all.Point_Data.X and Current_Dim = 0) then
            Find_Nearest_Helper(Node.all.Left, Target, Depth + 1);
            -- Check if we need to search the other side
            if abs (Target.X - Node.all.Point_Data.X) ** 2 < Min_Distance then
               Find_Nearest_Helper(Node.all.Right, Target, Depth + 1);
            end if;
         else
            Find_Nearest_Helper(Node.all.Right, Target, Depth + 1);
            -- Check if we need to search the other side
            if abs (Target.X - Node.all.Point_Data.X) ** 2 < Min_Distance then
               Find_Nearest_Helper(Node.all.Left, Target, Depth + 1);
            end if;
         end if;
      end Find_Nearest_Helper;
   begin
      if Tree.Root = null then
         return Target;  -- Return target if tree is empty
      end if;
      
      Find_Nearest_Helper(Tree.Root, Target, 0);
      return Nearest_Point;
   end Find_Nearest;

   -- Print the tree in-order traversal
   procedure Print_Tree(Node : KDNode_Access; Depth : Natural := 0) is
   begin
      if Node /= null then
         Print_Tree(Node.all.Left, Depth + 1);
         
         for I in 1..Depth loop
            Put("  ");
         end loop;
         
         Put("Point: (");
         Put(Node.all.Point_Data.X, Fore => 1, Aft => 2, Exp => 0);
         Put(", ");
         Put(Node.all.Point_Data.Y, Fore => 1, Aft => 2, Exp => 0);
         Put(") Dimension: ");
         Put(Node.all.Dimension);
         New_Line;
         
         Print_Tree(Node.all.Right, Depth + 1);
      end if;
   end Print_Tree;

   -- Example usage
   Tree : KDTree;
   Points : array(1..7) of Point := 
     ((X => 3.0, Y => 6.0),
      (X => 17.0, Y => 15.0),
      (X => 13.0, Y => 15.0),
      (X => 6.0, Y => 12.0),
      (X => 9.0, Y => 1.0),
      (X => 2.0, Y => 7.0),
      (X => 10.0, Y => 19.0));

begin
   -- Insert points into the tree
   Put_Line("Inserting points into K-D tree:");
   for I in Points'Range loop
      Insert(Tree, Points(I));
   end loop;

   -- Print the tree structure
   Put_Line("K-D Tree structure:");
   Print_Tree(Tree.Root);

   -- Test search functionality
   Put_Line("Searching for points:");
   
   -- Search for existing point
   declare
      Search_Point : constant Point := (X => 13.0, Y => 15.0);
   begin
      if Search(Tree, Search_Point) then
         Put_Line("Found point: (" & 
                  Float'Image(Search_Point.X) & ", " & 
                  Float'Image(Search_Point.Y) & ")");
      else
         Put_Line("Point not found: (" & 
                  Float'Image(Search_Point.X) & ", " & 
                  Float'Image(Search_Point.Y) & ")");
      end if;
   end;

   -- Search for non-existing point
   declare
      Search_Point : constant Point := (X => 5.0, Y => 5.0);
   begin
      if Search(Tree, Search_Point) then
         Put_Line("Found point: (" & 
                  Float'Image(Search_Point.X) & ", " & 
                  Float'Image(Search_Point.Y) & ")");
      else
         Put_Line("Point not found: (" & 
                  Float'Image(Search_Point.X) & ", " & 
                  Float'Image(Search_Point.Y) & ")");
      end if;
   end;

   -- Find nearest neighbor
   Put_Line("Finding nearest neighbor:");
   declare
      Target_Point : constant Point := (X => 8.0, Y => 8.0);
      Nearest : Point;
   begin
      Nearest := Find_Nearest(Tree, Target_Point);
      Put_Line("Nearest point to (" & 
               Float'Image(Target_Point.X) & ", " & 
               Float'Image(Target_Point.Y) & ") is: (" &
               Float'Image(Nearest.X) & ", " & 
               Float'Image(Nearest.Y) & ")");
   end;

end KDTree_Search;
```

## Key Features of this Implementation:

1. **Point Structure**: Defines 2D points with X and Y coordinates
2. **KDNode Structure**: Represents nodes in the K-D tree with point data, left/right children, and dimension information
3. **Insert Function**: Builds the tree by alternating dimensions (X,Y) at each level
4. **Search Function**: Finds exact matches in the tree
5. **Nearest Neighbor Search**: Implements efficient nearest neighbor search using branch-and-bound technique
6. **Tree Traversal**: Includes in-order traversal for visualization

## How it Works:

- The K-D tree alternates between dimensions (X and Y) when inserting nodes
- Search operations follow the appropriate branch based on the current dimension
- Nearest neighbor search uses a bounding box approach to avoid unnecessary searches
- The tree is built by comparing coordinates along the current dimension

This implementation demonstrates the core concepts of K-D tree algorithms in Ada, including tree construction, searching, and nearest neighbor queries.

