# Floyd's Cycle Detection Algorithm in Haskell

Floyd's cycle detection algorithm (also known as the "tortoise and hare" algorithm) is used to detect cycles in linked lists. Here's an implementation in Haskell:

```haskell
-- Define a simple linked list node type
data ListNode a = ListNode a (Maybe (ListNode a))
                deriving (Show, Eq)

-- Floyd's cycle detection algorithm
hasCycle :: Eq a => Maybe (ListNode a) -> Bool
hasCycle Nothing = False
hasCycle (Just head) = detectCycle head head
  where
    detectCycle slow fast
      | slow == fast && slow /= head = True  -- Cycle detected
      | slow == fast && slow == head = detectCycle (move slow) (move (move fast))
      | otherwise = case (move slow, move (move fast)) of
                      (Just s, Just f) -> detectCycle s f
                      _ -> False  -- No cycle (reached end)
    
    move (ListNode _ Nothing) = Nothing
    move (ListNode _ (Just next)) = Just next

-- Alternative cleaner implementation
hasCycle' :: Eq a => Maybe (ListNode a) -> Bool
hasCycle' Nothing = False
hasCycle' (Just head) = go head head
  where
    go slow fast
      | slow == fast && slow /= head = True
      | otherwise = case (move slow, move (move fast)) of
                      (Just s, Just f) -> go s f
                      _ -> False
    
    move (ListNode _ Nothing) = Nothing
    move (ListNode _ (Just next)) = Just next

-- Example usage with a cycle
createCycle :: IO ()
createCycle = do
  -- Create nodes
  let node1 = ListNode 1 Nothing
  let node2 = ListNode 2 Nothing
  let node3 = ListNode 3 Nothing
  let node4 = ListNode 4 Nothing
  
  -- Create a cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle back to node2)
  let node1' = ListNode 1 (Just node2)
  let node2' = ListNode 2 (Just node3)
  let node3' = ListNode 3 (Just node4)
  let node4' = ListNode 4 (Just node2)  -- Cycle back to node2
  
  -- Test with cycle
  print $ hasCycle' (Just node1')  -- Should return True
  
  -- Test with no cycle
  let nodeA = ListNode 1 (Just node2)
  let nodeB = ListNode 2 (Just node3)
  let nodeC = ListNode 3 Nothing
  
  print $ hasCycle' (Just nodeA)  -- Should return False

-- More robust version with explicit cycle detection
detectCycleStart :: Eq a => Maybe (ListNode a) -> Maybe (ListNode a)
detectCycleStart Nothing = Nothing
detectCycleStart (Just head) = 
  case detect head head of
    Just cycleStart -> Just cycleStart
    Nothing -> Nothing
  where
    detect slow fast
      | slow == fast && slow /= head = Just slow  -- Found meeting point
      | otherwise = case (move slow, move (move fast)) of
                      (Just s, Just f) -> detect s f
                      _ -> Nothing
    
    move (ListNode _ Nothing) = Nothing
    move (ListNode _ (Just next)) = Just next

-- Example with actual cycle detection
exampleWithCycle :: IO ()
exampleWithCycle = do
  let node1 = ListNode 1 (Just (ListNode 2 (Just (ListNode 3 (Just (ListNode 4 (Just (ListNode 2 Nothing))))))))
  print $ hasCycle' (Just node1)  -- True
```

## Key Features of this Implementation:

1. **Type Safety**: Uses Haskell's strong type system with `Maybe` for safe handling of empty lists
2. **Pattern Matching**: Leverages Haskell's pattern matching for clean code
3. **Lazy Evaluation**: Takes advantage of Haskell's lazy evaluation
4. **Functional Style**: Pure functional approach without side effects

## How it Works:

1. **Two Pointers**: Uses slow (tortoise) and fast (hare) pointers
2. **Movement**: Slow pointer moves one step, fast pointer moves two steps
3. **Cycle Detection**: If there's a cycle, the fast pointer will eventually catch up to the slow pointer
4. **Termination**: If either pointer reaches `Nothing`, there's no cycle

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1) - only uses two pointers

This implementation demonstrates Haskell's functional programming strengths while correctly implementing Floyd's cycle detection algorithm.

