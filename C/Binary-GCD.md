Here is a concise implementation of the **Binary GCD algorithm** (also known as Stein’s algorithm) in C. The function `binary_gcd` works for non‑negative integers and returns their greatest common divisor.

```c
/*  binary_gcd.c
 *  Compute GCD using the binary (Stein) algorithm.
 *
 *  Compile:  gcc -Wall -Wextra -o binary_gcd binary_gcd.c
 *  Run:      ./binary_gcd
 */

#include <stdio.h>

/*  Returns gcd(a, b) using the binary GCD algorithm.
 *  Works for a, b >= 0.  gcd(0,0) is defined as 0.
 */
unsigned int binary_gcd(unsigned int a, unsigned int b)
{
    /* GCD(0, b) == b; GCD(a, 0) == a */
    if (a == 0) return b;
    if (b == 0) return a;

    /* Find common power of two */
    unsigned int shift = 0;
    while (((a | b) & 1U) == 0U) {   // both even
        a >>= 1;
        b >>= 1;
        ++shift;
    }

    /* Make a odd */
    while ((a & 1U) == 0U)
        a >>= 1;

    /* From here, a is always odd */
    do {
        /* Remove all factors of 2 in b – they are not common */
        while ((b & 1U) == 0U)
            b >>= 1;

        /* Now a and b are both odd. Swap if necessary so a <= b */
        if (a > b) {
            unsigned int t = a;
            a = b;
            b = t;
        }
        b = b - a;   // b becomes even (or zero)
    } while (b != 0);

    /* Restore common factors of 2 */
    return a << shift;
}

/* Simple driver to demonstrate usage */
int main(void)
{
    unsigned int x = 48, y = 180;
    printf("gcd(%u, %u) = %u\n", x, y, binary_gcd(x, y));

    /* Test edge cases */
    printf("gcd(0, 5) = %u\n", binary_gcd(0, 5));
    printf("gcd(0, 0) = %u\n", binary_gcd(0, 0));
    printf("gcd(12345678, 87654321) = %u\n",
           binary_gcd(12345678, 87654321));

    return 0;
}
```

### How it works
1. **Remove common factors of 2** – shift both numbers right until at least one is odd, counting how many shifts (`shift`) we performed.
2. **Make the first number odd** – keep shifting `a` right until it becomes odd.
3. **Loop**:
   - Strip factors of 2 from `b` (making it odd).
   - Ensure `a ≤ b` by swapping if necessary.
   - Replace `b` with `b - a` (which is even).
   - Repeat until `b` becomes zero.
4. **Restore the common power of two** – left‑shift the resulting `a` by `shift` positions.

The algorithm runs in **O(log min(a,b))** time and uses only bitwise operations, making it efficient on platforms where division/modulo is costly.