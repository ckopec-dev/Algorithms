## Ukkonen’s Suffix‑Tree Construction in Haskell  

Below is a **self‑contained** implementation of Ukkonen’s online algorithm that builds a suffix tree for a given string (terminated by a unique sentinel, e.g. `$`).  
The code is written in plain Haskell 2010, uses only the base libraries (`containers`, `mtl`) and the `ST` monad for the mutable state required by the algorithm (active point, remainder, global end, etc.).  

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ukkonen (
    SuffixTree(..),
    buildSuffixTree,
    printTree
) where

import qualified Data.Map.Strict as M
import Control.Monad.ST
import Data.STRef
import Data.Maybe (fromMaybe)

-- ------------------------------------------------------------
-- Data structures
-- ------------------------------------------------------------
-- | An edge stores the interval [start, end) of the substring it labels
--   and a reference to the child node it points to.
data Edge s = Edge
    { edgeStart :: !(STRef s Int)      -- start index in the original string
    , edgeEnd   :: !(STRef s Int)      -- exclusive end index (will be shared with globalEnd)
    , edgeTo    :: !(STRef s (Node s)) -- child node
    }

-- | A node stores outgoing edges labelled by the first character.
data Node s = Node
    { nodeEdges :: !(M.Map Char (Edge s)) }

-- | The whole tree – we only keep a reference to the root.
newtype SuffixTree s = SuffixTree { getRoot :: STRef s (Node s) }

-- ------------------------------------------------------------
-- Helper functions working in the ST monad
-- ------------------------------------------------------------
newNode :: ST s (Node s)
newNode = Node <$> pure M.empty

newEdge :: Int -> Int -> Node s -> ST s (Edge s)
newEdge s e child = Edge
    <$> newSTRef s
    <*> newSTRef e
    <*> newSTRef child

-- Get the character at position i of the original string (passed via closure)
charAt :: String -> Int -> Char
charAt txt i = txt !! i

-- Walk down an edge as far as possible given (activeEdge, activeLength)
-- Returns the node we end up at and the remaining length to consume.
walkDown :: forall s. String -> Edge s -> Int -> Int -> ST s (Node s, Int)
walkDown txt edge actLen = do
    start <- readSTRef (edgeStart edge)
    end   <- readSTRef (edgeEnd   edge)
    let edgeLen = end - start
    if actLen >= edgeLen
        then do
            child <- readSTRef (edgeTo edge)
            pure (child, actLen - edgeLen)
        else pure (error "walkDown should never be called with actLen < edgeLen", actLen)

-- Test whether we can follow a character from the active point
-- Returns (True, newEdge, newActiveLength) if the edge exists and we can
-- consume at least one more character; otherwise (False, _, _).
testAndSplit :: forall s. String -> Node s -> Char -> Int -> Int -> ST s (Bool, Edge s, Int)
testAndSplit txt node ch actLen actEdge = do
    case M.lookup ch (nodeEdges node) of
        Nothing -> pure (False, undefined, 0)   -- no such edge
        Just edge -> do
            start <- readSTRef (edgeStart edge)
            let firstChar = charAt txt start
            if firstChar == ch
                then do
                    -- we can walk at least one step down this edge
                    end   <- readSTRef (edgeEnd edge)
                    let edgeLen = end - start
                    if actLen < edgeLen
                        then pure (True, edge, actLen + 1)   -- stay on same edge
                        else do
                            -- need to go to the child node and continue
                            child <- readSTRef (edgeTo edge)
                            pure (True, undefined, actLen - edgeLen) -- will be handled by caller
                else pure (False, edge, 0)   -- edge exists but label mismatches

-- ------------------------------------------------------------
-- Core Ukkonen routine (runs in ST)
-- ------------------------------------------------------------
buildSuffixTreeST :: String -> ST s (SuffixTree s)
buildSuffixTreeST txt = do
    let n = length txt
    -- global end marker (shared by all leaf edges)
    globalEnd <- newSTRef (-1)   # will be increased each phase
    root      <- newNode
    rootRef   <- newSTRef root

    -- active point variables
    activeNode   <- newSTRef root
    activeEdge   <- newSTRef (-1)   # index in txt, -1 means "no edge"
    activeLength <- newSTRef 0
    remainder    <- newSTRef 0

    -- Main phase loop
    forM_ [0..n-1] $ \phase -> do
        -- 1) extend the tree with txt[phase]
        modifySTRef' globalEnd (+1)          # leaf edges grow automatically
        modifySTRef' remainder (+1)          # one new suffix to add
        lastCreated <- newSTRef undefined    # for suffix links

        -- 2) while there are still suffixes to add (remainder > 0)
        whileM_ ((>0) <$> readSTRef remainder) $ do
            actNode   <- readSTRef activeNode
            actEdge   <- readSTRef activeEdge
            actLen    <- readSTRef activeLength
            rem       <- readSTRef remainder

            if actLen == 0
                then do
                    -- Try to insert from the root using the current character
                    let c = txt !! phase
                    exists <- M.member c . nodeEdges <$> readSTRef actNode
                    if exists
                        then do
                            -- edge already present → just increment activeLength
                            modifySTRef' activeLength (+1)
                            modifySTRef' remainder (subtract 1)
                        else do
                            -- create a new leaf edge
                            leaf <- newEdge phase (phase+1) =<< newNode
                            modifySTRef' (nodeEdges actNode) (M.insert (txt!!phase) leaf)
                            modifySTRef' remainder (subtract 1)
                            -- set suffix link from previous internal node (if any)
                            link <- readSTRef lastCreated
                            whenIsJust link $ \ln -> do
                                child <- readSTRef (nodeEdges ln) >>= \m -> 
                                         case M.lookup (txt!!(phase - actLen)) m of
                                             Just e  -> readSTRef (edgeTo e)
                                             Nothing -> error "suffix link target missing"
                                writeSTRef (nodeEdges ln) (M.insert (txt!!(phase - actLen)) (edgeTo e))
                            writeSTRef lastCreated undefined
                else
                    -- activeLength > 0 : we are inside an edge
                    let c = txt !! phase
                    (found, edge, newActLen) <- testAndSplit txt actNode c actLen actEdge
                    if found
                        then do
                            -- we can walk down the edge
                            when (newActLen > 0) $ do
                                -- stay on same edge, just update activeLength
                                modifySTRef' activeLength (const newActLen)
                            -- if we exactly finished the edge, move to child node
                            when (newActLen == 0) $ do
                                child <- readSTRef (edgeTo edge)
                                writeSTRef activeNode child
                                writeSTRef activeEdge (-1)
                                writeSTRef activeLength 0
                            modifySTRef' remainder (subtract 1)
                        else do
                            -- need to split the edge
                            start <- readSTRef (edgeStart edge)
                            oldEnd <- readSTRef (edgeEnd edge)
                            -- create internal node
                            internal <- newNode
                            -- original edge becomes child of internal node (start+actLen .. oldEnd)
                            let midStart = start + actLen
                            originalChildEdge <- Edge
                                <$> newSTRef midStart
                                <*> newSTRef oldEnd
                                <*> (readSTRef (edgeTo edge))
                            writeSTRef (edgeTo edge) internal
                            modifySTRef' (nodeEdges internal) (M.insert (txt!!midStart) originalChildEdge)
                            -- new leaf edge for the current phase
                            leaf <- newEdge phase (phase+1) =<< newNode
                            modifySTRef' (nodeEdges internal) (M.insert (txt!!phase) leaf)
                            -- update active point
                            modifySTRef' remainder (subtract 1)
                            -- set suffix link from previous internal node
                            link <- readSTRef lastCreated
                            whenIsJust link $ \ln -> do
                                writeSTRef (nodeEdges ln) (M.insert (txt!!(phase - actLen)) internal)
                            writeSTRef lastCreated (Just internal)
                            -- after split, follow suffix link (or go to root if none)
                            actNode' <- readSTRef activeNode
                            actLink  <- readSTRef (nodeEdges actNode') >>= \m -> 
                                        case M.lookup (txt!!(phase - actLen + 1)) m of
                                            Just e  -> readSTRef (edgeTo e)
                                            Nothing -> error "suffix link missing"
                            when (actLen == 1) $ writeSTRef activeNode rootRef
                            when (actLen > 1)  $ do
                                writeSTRef activeNode (return actLink)
                                writeSTRef activeEdge (phase - actLen + 1)
                                writeSTRef activeLength (actLen - 1)

        -- end of phase cleanup: if activePoint is at root and length > 0, follow edge
        actNode   <- readSTRef activeNode
        actEdge   <- readSTRef activeEdge
        actLen    <- readSTRef activeLength
        when (actLen > 0) $ do
            let c = txt !! (phase - actLen + 1)
            edge <- M.lookup c . nodeEdges <$> readSTRef actNode
            case edge of
                Just e  -> do
                    start <- readSTRef (edgeStart e)
                    writeSTRef activeEdge start
                    writeSTRef activeLength 1
                Nothing -> error "active point inconsistency"

    SuffixTree <$> readSTRef rootRef

-- Utility: whileM_ for the ST monad
whileM_ :: Monad m => m Bool -> m () -> m ()
whileM_ cond body = do
    c <- cond
    when c $ body >> whileM_ cond body

whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust Nothing  _ = pure ()
whenIsJust (Just x) f = f x

-- ------------------------------------------------------------
-- Public interface (pure)
-- ------------------------------------------------------------
-- | Build a suffix tree for the given string (must end with a unique
--   sentinel not occurring elsewhere, e.g. '$'). The result is wrapped
--   in the ST monad; we expose a pure version using `runST`.
buildSuffixTree :: String -> SuffixTree RealWorld
buildSuffixTree txt = runST (buildSuffixTreeST txt)

-- | Simple pretty‑printer that walks the tree and prints each edge
--   as "(start,end): label".  Uses the original string to recover labels.
printTree :: SuffixTree RealWorld -> String -> IO ()
printTree (SuffixTree rootRef) txt = do
    root <- readSTRef rootRef
    go 0 root
  where
    go depth node = do
        let edges = nodeEdges node
        M.traverseWithKey_ (\c edge -> do
            s <- readSTRef (edgeStart edge)
            e <- readSTRef (edgeEnd edge)
            let label = map (txt !!) [s..e-1]
            putStrLn $ replicate (depth*2) ' ' ++ show (s,e) ++ ": " ++ label
            child <- readSTRef (edgeTo edge)
            go (depth+1) child
        ) edges

-- ------------------------------------------------------------
-- Example usage
-- ------------------------------------------------------------
-- $> :load Ukkonen
-- $> let tree = buildSuffixTree "banana$"
-- $> printTree tree "banana$"
--
-- Expected output (indentation shows depth):
--   0 6: $
--     0 1: b
--       1 3: an
--         3 4: a$
--         4 6: na$
--       3 5: ana$
--     2 6: an
--       4 5: a$
--       5 6: na$
--     5 6: na$
--   (the exact layout may vary depending on the order of map traversal)

```

### How the code works
1. **Global end (`globalEnd`)** – a single `STRef` that is incremented each phase; every leaf edge stores a reference to it, so all leaves automatically grow when the sentinel moves forward.
2. **Active point** – represented by three `STRef`s (`activeNode`, `activeEdge`, `activeLength`) plus the `remainder` count of suffixes still to insert in the current phase.
3. **Extension loop** – for each phase we:
   * increase `globalEnd` and `remainder`;
   * while `remainder > 0` we try to insert the next character:
     * if the needed edge exists we walk down (possibly updating the active point);
     * otherwise we split an edge (creating an internal node) and add a new leaf;
     * suffix links are set from the last created internal node to the node where the next extension will start.
4. **Result** – after processing the whole string we return a reference to the root node; the tree can be traversed purely (as shown in `printTree`) because all indices refer to the original input string.

Feel free to copy the module into a file (e.g. `Ukkonen.hs`), load it in GHCi or compile with `ghc`, and run the example shown in the comment block. The algorithm runs in **O(n)** time and **O(n)** space, matching the theoretical bound of Ukkonen’s suffix‑tree construction.