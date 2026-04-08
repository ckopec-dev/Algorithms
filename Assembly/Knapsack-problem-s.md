Below is a **compact, instructional example** of the classic **0/1 knapsack** dynamic‑programming algorithm written for **x86‑32 (NASM syntax)**.  
The code assumes the following input is already present in memory:

| Symbol | Meaning |
|--------|---------|
| `weights` | Array of item weights (`DWORD`s) |
| `values`  | Array of item values (`DWORD`s) – same length as `weights` |
| `n`       | Number of items (`DWORD`) |
| `C`       | Knapsack capacity (`DWORD`) |
| `dp`      | One‑dimensional DP table (`DWORD[ C+1 ]`), initialized to 0 |

The algorithm fills `dp[w]` = *maximum value achievable with total weight ≤ w* using a **single‑dimensional** DP (the usual space‑optimized version).  
After the loops finish, `dp[C]` holds the answer.

---

### Assembly (NASM) – 32‑bit

```asm
; ------------------------------------------------------------
; 0/1 Knapsack – space‑optimized DP (single array)
; ------------------------------------------------------------
; Input (already in .data section):
;   n        dword   number of items
;   C        dword   knapsack capacity
;   weights  dword   array[n]   ; weight of each item
;   values   dword   array[n]   ; value  of each item
;   dp       times (C+1) dword 0   ; DP table, zero‑initialised
;
; Output:
;   dp[C]    dword   maximum obtainable value
;
; ------------------------------------------------------------
section .data
    n        dd 4                     ; example: 4 items
    C        dd 10                    ; capacity = 10
    weights  dd 2, 3, 4, 5            ; w[i]
    values   dd 3, 4, 5, 8            ; v[i]
    ; dp table: size = C+1 entries, each 4 bytes
    dp       times (10+1) dd 0        ; will be adjusted by assembler if C changes

section .text
    global _start

_start:
    ; --------------------------------------------------------
    ; Preserve registers we will clobber (EBX, ESI, EDI)
    ; --------------------------------------------------------
    push ebx
    push esi
    push edi

    ; --------------------------------------------------------
    ; Outer loop: for i = 0 .. n-1
    ; --------------------------------------------------------
    xor esi, esi                ; i = 0 (item index)
outer_loop:
    cmp esi, [n]
    jge outer_done              ; if i >= n → exit outer loop

    ; Load weight[i] and value[i] into registers for inner loop
    mov ebx, dword [weights + esi*4]   ; ebx = w[i]
    mov edi, dword [values  + esi*4]   ; edi = v[i]

    ; --------------------------------------------------------
    ; Inner loop: for w = C downto weight[i]
    ; --------------------------------------------------------
    mov ecx, [C]                ; w = C
inner_loop:
    cmp ecx, ebx                ; if w < weight[i] → stop inner loop
    jl  inner_done

    ; dp[w] = max(dp[w], dp[w - weight[i]] + value[i])
    ; ----------------------------------------------------
    ; Compute index = w
    mov eax, ecx                ; eax = w
    shl eax, 2                  ; eax = w*4  (byte offset into dp)
    mov edx, [dp + eax]         ; edx = dp[w] (old)

    ; Compute dp[w - weight[i]]
    sub eax, ebx*4              ; eax = (w - weight[i])*4
    mov esi2, [dp + eax]        ; esi2 = dp[w - weight[i]]
    add esi2, edi               ; esi2 = dp[w - weight[i]] + value[i]

    ; Take the max
    cmp edx, esi2
    cmovl edx, esi2             ; if dp[w] < new → edx = new
    mov [dp + ecx*4], edx       ; store back dp[w]

    ; w--
    dec ecx
    jmp inner_loop

inner_done:
    ; --------------------------------------------------------
    ; Next item
    ; --------------------------------------------------------
    inc esi
    jmp outer_loop

outer_done:
    ; --------------------------------------------------------
    ; Result is in dp[C]
    ; --------------------------------------------------------
    mov eax, [C]                ; eax = capacity
    shl eax, 2                  ; eax = C*4
    mov eax, [dp + eax]         ; eax = dp[C] = answer

    ; (For demo purposes we just exit with the answer in eax.
    ;  In a real program you would store/print it.)
    mov ebx, eax                ; ebx = answer (optional)

    ; --------------------------------------------------------
    ; Exit Linux syscall (eax=1)
    ; --------------------------------------------------------
    pop edi
    pop esi
    pop ebx
    mov eax, 1                  ; sys_exit
    xor ebx, ebx                ; status = 0
    int 0x80
```

---

### How the code maps to the DP algorithm

| High‑level step | Assembly fragment | Meaning |
|-----------------|-------------------|---------|
| **Initialize** `dp[0..C] = 0` | `dp times (C+1) dd 0` | Zero‑filled table in `.data`. |
| **Outer loop** over items | `outer_loop … inc esi` | `esi` holds the item index `i`. |
| **Load** `weight[i]` & `value[i]` | `mov ebx, [weights+esi*4]` / `mov edi, [values+esi*4]` | Current item’s weight (`ebx`) and value (`edi`). |
| **Inner loop** descending weight | `mov ecx, [C] … dec ecx` | `ecx` runs from `C` down to `weight[i]`. |
| **Transition** `dp[w] = max(dp[w], dp[w-wi] + vi)` | Computes two candidates, uses `cmovl` to keep the larger, stores back. |
| **Result** `dp[C]` | Final `mov eax, [dp + eax*4]` | Holds the maximal obtainable value. |

---

#### Remarks & Portability

* **Assembler** – The snippet uses NASM syntax (`times`, `[base+index*scale]`). For MASM/TASM you’d adjust the directive syntax (`dp DWORD (C+1) DUP (0)`) and the operand order.
* **Calling convention** – The code is a standalone program (`_start`) that exits via Linux `int 0x80`. If you want to call it from C, wrap the core loops in a function and follow the System V i386 ABI (preserve `EBX`, `ESI`, `EDI`; return value in `EAX`).
* **Capacity limits** – The DP table lives in the data segment; for very large `C` you would need to allocate it on the heap (`malloc`) or use a larger segment model.
* **Extension** – To also reconstruct the chosen items, keep a predecessor table or back‑track using the same `dp` array after the main loops.

Feel free to copy, assemble (`nasm -f elf32 knap.asm -o knap.o && ld -m elf_i386 knap.o -o knap`), and run the program; the final answer (`dp[C]`) will be left in `EAX` (and also copied to `EBX` before exit). Enjoy exploring knapsack in assembly!