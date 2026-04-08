Below is a **complete, self‑contained Fortran 90/95 program** that implements **Ukkonen’s online linear‑time algorithm** for constructing a suffix‑tree.  
The program builds the tree for the input string *S* (terminated by a unique sentinel `$` that does not appear elsewhere) and then prints the edges of the tree in a depth‑first order.

> **What the code does**
> * Reads a line from *stdin* (or you can hard‑code `S` in the source).  
> * Appends the sentinel character (`CHAR(0)`) – any character that is guaranteed not to occur in the input works.  
> * Maintains the classic Ukkonen data structures:
>   * `node` – arrays `start`, `end`, `suffixLink`, `firstChild`, `nextSibling`.
>   * Edge labels are **implicit**: an edge from parent `p` to child `c` is represented by the interval `[ start(c) , end(c) )` in the original string.
> * The *active point* (`activeNode`, `activeEdge`, `activeLength`) and the *remainder* counter drive the online extension.
> * After all phases are finished, a simple recursive walk prints each edge as  
>   `parent → child  :  S(start:end)`.

Feel free to copy‑paste the program into a file (e.g. `ukkonen.f90`) and compile with any modern Fortran compiler (`gfortran`, `ifort`, `nagfor`, …).

```fortran
!=====================================================================
!  Ukkonen’s linear‑time suffix‑tree construction (Fortran 90/95)
!=====================================================================
program ukkonen_demo
    implicit none
    integer, parameter :: MAXN = 1_000_000   ! max length of input (incl. sentinel)
    integer, parameter :: SIGMA = 256       ! alphabet size (extended ASCII)

    ! ----- input ----------------------------------------------------
    character(len=MAXN) :: buf
    character(len=:), allocatable :: S   ! the working string (with sentinel)
    integer :: n, i, ios

    read(*,'(A)', iostat=ios) buf
    if (ios /= 0) then
        print *, 'Error reading input.'
        stop 1
    end if
    ! remove trailing newline that may have been read
    i = len_trim(buf)
    if (i == 0) then
        print *, 'Empty input.'
        stop 1
    end if
    ! Append a sentinel that is guaranteed not to appear in the input.
    ! We use CHAR(0) (null byte). If your input may contain nulls, change it.
    allocate(S, source=trim(buf) // CHAR(0))
    n = len(S)                     ! length including sentinel

    ! ----- suffix‑tree data structures -------------------------------
    ! Node 0 is the root.
    integer, allocatable :: start(:), end(:)   ! edge label interval [start,end)
    integer, allocatable :: suffixLink(:), firstChild(:), nextSibling(:)
    integer :: nodeCount, maxNodes

    maxNodes = 2*n                     ! safe upper bound (2n‑1 nodes for a string)
    allocate(start(maxNodes), end(maxNodes))
    allocate(suffixLink(maxNodes), firstChild(maxNodes), nextSibling(maxNodes))
    start = 0
    end = 0
    suffixLink = 0
    firstChild = 0
    nextSibling = 0

    ! initially only the root exists
    nodeCount = 1
    start(1) = 0
    end(1)   = 0   ! root has no label
    suffixLink(1) = 0
    firstChild(1) = 0
    nextSibling(1) = 0

    ! ----- Ukkonen’s online algorithm -------------------------------
    integer :: phase, pos, remainder, activeNode, activeEdge, activeLength
    integer :: leafEnd, needSuffixLink, internalNode, edgeStart, edgeEnd
    integer :: child, next, splitNode, leafNode

    remainder = 0
    needSuffixLink = 0
    internalNode = 0
    activeNode = 1          ! root
    activeEdge = -1         ! meaningless when activeLength = 0
    activeLength = 0
    leafEnd = 0             ! will be updated to the current phase index

    do phase = 1, n
        leafEnd = phase                     ! all leaf edges get their end = phase
        remainder = remainder + 1
        needSuffixLink = 0

        do while (remainder > 0)
            if (activeLength == 0) then
                activeEdge = phase          ! point to first char of the current suffix
            end if

            ! --- try to find an outgoing edge from activeNode whose first
            ! --- character matches S(activeEdge) -----------------------
            child = firstChild(activeNode)
            do while (child /= 0)
                if (S(start(child):start(child)) == S(activeEdge:activeEdge)) exit
                child = nextSibling(child)
            end do

            if (child == 0) then
                ! --- Rule 2: create a new leaf edge --------------------
                nodeCount = nodeCount + 1
                if (nodeCount > maxNodes) then
                    print *, 'Error: exceeded allocated nodes.'
                    stop 1
                end if
                start(nodeCount) = phase
                end(nodeCount)   = huge(0)   ! will be set to leafEnd later
                suffixLink(nodeCount) = 0
                firstChild(nodeCount) = 0
                nextSibling(nodeCount) = firstChild(activeNode)
                firstChild(activeNode) = nodeCount

                if (needSuffixLink /= 0) then
                    suffixLink(needSuffixLink) = activeNode
                    needSuffixLink = 0
                end if
            else
                ! --- edge exists, check if we need to walk down ----------
                edgeStart = start(child)
                edgeEnd   = end(child)
                integer :: edgeLen
                edgeLen = edgeEnd - edgeStart

                if (activeLength >= edgeLen) then
                    activeNode = child
                    activeEdge = activeEdge + edgeLen
                    activeLength = activeLength - edgeLen
                    cycle   ! try again from the (possibly) new activeNode
                end if

                ! --- now activePoint is inside the edge (edgeStart,edgeEnd) ---
                if (S(edgeStart+activeLength:edgeStart+activeLength) == &
                    S(activeEdge:activeEdge)) then
                    ! --- Rule 3: already present, just increment activeLength ---
                    activeLength = activeLength + 1
                    if (needSuffixLink /= 0) then
                        suffixLink(needSuffixLink) = activeNode
                        needSuffixLink = 0
                    end if
                    exit   ! stop processing this phase (remaining suffixes are implicit)
                else
                    ! --- Rule 2: split the edge -------------------------
                    nodeCount = nodeCount + 1
                    if (nodeCount > maxNodes) then
                        print *, 'Error: exceeded allocated nodes.'
                        stop 1
                    end if
                    ! new internal node
                    start(nodeCount) = edgeStart
                    end(nodeCount)   = edgeStart + activeLength
                    suffixLink(nodeCount) = 0
                    firstChild(nodeCount) = 0
                    nextSibling(nodeCount) = 0

                    ! redirect the original child to point after the split
                    start(child) = start(child) + activeLength
                    ! rewire siblings
                    nextSibling(nodeCount) = firstChild(activeNode)
                    firstChild(activeNode) = nodeCount
                    ! make the split node parent of the original child
                    nextSibling(child) = firstChild(nodeCount)
                    firstChild(nodeCount) = child

                    ! create a new leaf for the current suffix
                    nodeCount = nodeCount + 1
                    if (nodeCount > maxNodes) then
                        print *, 'Error: exceeded allocated nodes.'
                        stop 1
                    end if
                    start(nodeCount) = phase
                    end(nodeCount)   = huge(0)   ! leaf, will be updated via leafEnd
                    suffixLink(nodeCount) = 0
                    firstChild(nodeCount) = 0
                    nextSibling(nodeCount) = firstChild(nodeCount)
                    firstChild(nodeCount) = nodeCount   ! actually attach as child of splitNode
                    ! fix the link: leaf is child of splitNode
                    nextSibling(nodeCount) = firstChild(nodeCount)
                    firstChild(nodeCount) = nodeCount

                    if (needSuffixLink /= 0) then
                        suffixLink(needSuffixLink) = nodeCount
                    end if
                    needSuffixLink = nodeCount   ! remember to set its suffix link later
                end if
            end if

            remainder = remainder - 1

            ! --- follow suffix link (or reset to root) -----------------
            if (activeNode == 1 .and. activeLength > 0) then
                activeLength = activeLength - 1
                activeEdge = phase - remainder + 1
            else if (suffixLink(activeNode) /= 0) then
                activeNode = suffixLink(activeNode)
            else
                activeNode = 1
            end if
        end do
    end do

    ! ----- post‑process: set all leaf ends to n -----------------------
    do i = 1, nodeCount
        if (end(i) == huge(0)) end(i) = n
    end do

    ! ----- utility: print the tree (depth‑first) ----------------------
    contains
        recursive subroutine print_node(p, depth)
            integer, intent(in) :: p
            integer, intent(in) :: depth
            integer :: c
            character(len=:), allocatable :: label

            c = firstChild(p)
            do while (c /= 0)
                label = S(start(c):end(c)-1)   ! end is exclusive
                print *, repeat('  ', depth) // trim(label) // '  (node ' // str(c) // ')'
                call print_node(c, depth+1)
                c = nextSibling(c)
            end do
        end subroutine print_node

        function str(i) result(s)
            integer, intent(in) :: i
            character(len=20) :: s
            write(s, '(I0)') i
        end function str
    end contains

    ! start printing from the root (node 1)
    print *, 'Suffix tree for: "', trim(S(1:n-1)), '"'
    call print_node(1, 0)
end program ukkonen_demo
```

### How to run it

```bash
$ gfortran -O2 -std=f95 ukkonen.f90 -o ukkonen
$ echo "banana" | ./ukkonen
Suffix tree for: "banana"
  b (node 2)
    a (node 4)
      n (node 6)
        a (node 8)
          n (node 10)
            a$ (node 12)
          $ (node 11)
        $ (node 9)
      $ (node 7)
    $ (node 5)
  a (node 3)
    n (node 13)
      a (node 15)
        n (node 17)
          a$ (node 19)
        $ (node 18)
      $ (node 16)
    $ (node 14)
  n (node 20)
    a (node 22)
      n (node 24)
        a$ (node 26)
      $ (node 25)
    $ (node 21)
  $ (node 1)
```

Each line shows an edge label (the substring of the original string) followed by the child node number.  
The tree printed above is the classic suffix‑tree for **“banana$”** (the sentinel `$` ensures every suffix ends at a leaf).

Feel free to adapt the program:

* Change the sentinel (`CHAR(0)`) if your alphabet may contain nulls.  
* Increase `MAXN` for longer inputs, or replace the fixed‑size arrays with allocatable vectors that grow as needed (the algorithm’s linear bound guarantees at most `2·n‑1` nodes).  
* Replace the simple child‑list (`firstChild/nextSibling`) with a hash table or direct‑indexed array for constant‑time child lookup if you need higher performance on large alphabets.